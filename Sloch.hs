{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Debug.Trace

import ByteStringUtils
import LangInfo

import Control.Lens ((.=), (%=), (^.), makeLenses, set, use, uses)
import Control.Monad.State
import Data.List (intercalate)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.Exit (ExitCode(..), exitWith)

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map                   as Map

type Sloch = State FileState

-- The state of a file being filtered, including what language it's in.
--
data FileState =
   FileState { _fsLangInfo         :: LangInfo       -- The language info of the file.
             , _fsContents         :: [BL.ByteString] -- The remaining unfiltered lines.
             , _fsFilteredContents :: [BL.ByteString] -- The lines filtered thus far.
             , _fsInBlockComment   :: Bool           -- Whether or not we are inside a block comment.
             }

makeLenses ''FileState

-- initFileState contents
--
-- Creates a FileState from a file as a ByteString
--
initFileState :: LangInfo -> BL.ByteString -> FileState
initFileState lang_info contents =
   FileState { _fsLangInfo   = lang_info
             , _fsContents   = BL.lines contents
             , _fsFilteredContents = []
             , _fsInBlockComment = False
             }

-- POSIX regex escape characters.
--
regexEscapeChars :: String
regexEscapeChars = ".^$*+?()[{\\"

langInfoMap :: Map.Map String LangInfo
langInfoMap = Map.fromList [ ("c",  cLangInfo      )
                           , ("hs", haskellLangInfo)
                           ]

-- slouch target
--
-- Counts the SLOC in |target| and prints to stdout.
--
slouch :: FilePath -> IO ()
slouch target = do
   file_exists      <- doesFileExist      target
   directory_exists <- doesDirectoryExist target

   -- TODO: MultiWayIf pragma would clean this up
   case () of
      _ | file_exists      -> slochFile      target
      _ | directory_exists -> slochDirectory target
        | otherwise        -> error $ "'" ++ target ++ "' does not exist"

-- slochFile target
--
-- Counts the SLOC in |target| and prints to stdout.
--
slochFile :: FilePath -> IO ()
slochFile target = do
   let ext = drop 1 $ takeExtension target -- drop '.'

   case Map.lookup ext langInfoMap of
      Just lang_info -> do
         contents <- BL.readFile target
         let sloc = filterContents lang_info contents
         mapM_ (putStrLn . BL.unpack) sloc
         putStrLn $ "Lines: " ++ show (length sloc)
      Nothing -> do
         putStrLn $ "Unknown file extension: '" ++ ext ++ "'"
         exitWith $ ExitFailure 1

slochDirectory :: FilePath -> IO ()
slochDirectory target = do
   contents <- getDirectoryContents_ target
   forM_ contents slouch

-- getDirectoryContents_ target
--
-- Gets directory contents from |target|, less "." and ".."
--
getDirectoryContents_ :: FilePath -> IO [FilePath]
getDirectoryContents_ target = do
   contents <- getDirectoryContents target
   return $ filter (`notElem` [".", ".."]) contents

-- filterContents lang_info contents
--
-- Filters comments out of |contents| (a flat bytestring) per |lang_info|,
-- returning the filtered contents as a list of strings.
--
filterContents :: LangInfo -> BL.ByteString -> [BL.ByteString]
filterContents lang_info contents = final_file_state ^. fsFilteredContents
   where init_file_state :: FileState
         init_file_state = initFileState lang_info contents

         final_file_state :: FileState
         final_file_state = execState filterContents_ init_file_state

filterContents_ :: Sloch ()
filterContents_ = do
   file_contents <- use fsContents
   case file_contents of
      [] -> return ()
      (x:xs) -> do
         x' <- removeLineComments x >>=
               removeBlockComments  >>=
               removeBoilerPlate . bStrip    -- strip before matching boiler plate

         fsContents         .= xs
         fsFilteredContents %= (++ [x'])  -- TODO: Can this be refactored to use (:)?
         filterContents_

removeLineComments :: BL.ByteString -> Sloch BL.ByteString
removeLineComments line = do
   line_comment <- uses fsLangInfo lineComment
   let (line', _) = B.breakSubstring (lazyToStrictBS line_comment) (lazyToStrictBS line)
   return $ BL.fromChunks [line']

removeBlockComments :: BL.ByteString -> Sloch BL.ByteString
removeBlockComments = return

removeBoilerPlate :: BL.ByteString -> Sloch BL.ByteString
removeBoilerPlate = return

-- removeBoilerPlate lang_info contents
--
-- Removes boiler plate lines from |contents| per the boiler plate matching
-- rules in |lang_info|.
--
{-removeBoilerPlate :: LangInfo -> [BL.ByteString] -> [BL.ByteString]-}
{-removeBoilerPlate lang_info contents =-}
   {-foldr applyFilter contents (boilerPlate lang_info)-}

   {-where-}
      {-applyFilter :: LineFilter -> [BL.ByteString] -> [BL.ByteString]-}
      {-applyFilter f = filter (not . f)-}

--------------------------------------------------------------------------------

data Options = Options
   { _optShowHelp    :: Bool
   , _optShowVersion :: Bool
   , _optVerbose     :: Bool
   } deriving Show

makeLenses ''Options

defaultOptions :: Options
defaultOptions = Options
   { _optShowHelp    = False
   , _optShowVersion = False
   , _optVerbose     = False
   }

options :: [OptDescr (Options -> Options)]
options = [ Option "?" ["help"]    (NoArg $ set optShowHelp    True) "show help"
          , Option "v" ["verbose"] (NoArg $ set optVerbose     True) "verbose output"
          , Option "V" ["version"] (NoArg $ set optShowVersion True) "show version"
          ]

usage :: String
usage = "Usage: sloch (filename | dirname)" -- ++ concatMap show options

parseOptions :: [String] -> IO (Options, [String])
parseOptions args =
   case getOpt Permute options args of
      (opts, non_opts, []) ->
         case length non_opts of
            0 -> error usage
            1 -> return (foldl (flip id) defaultOptions opts, non_opts)
            _ -> error  $ "Unrecognized arguments: " ++ intercalate ", " (tail non_opts)
      (_, _, errors) -> error $ concat errors ++ usage

main :: IO ()
main = do
   (opts, [target]) <- getArgs >>= parseOptions
   print opts
   slouch target
