{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Debug.Trace

import ByteStringUtils
import LangInfo

import Control.Applicative ((<*>), pure)
import Control.Lens ((.=), (%=), (^.), both, makeLenses, over, set, use)
import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>), takeExtension)

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map                   as Map

type Sloch = State FileState

-- The state of a file being filtered, including what language it's in.
--
data FileState =
   FileState { _fsLangInfo         :: LangInfo        -- The language info of the file.
             , _fsContents         :: [BL.ByteString] -- The remaining unfiltered lines.
             , _fsFilteredContents :: [BL.ByteString] -- The lines filtered thus far.
             , _fsInBlockComment   :: Bool            -- Whether or not we are inside a block comment.
             }

makeLenses ''FileState

-- initFileState contents
--
-- Creates a FileState from a file as a ByteString.
--
initFileState :: LangInfo -> BL.ByteString -> FileState
initFileState lang_info contents =
   FileState { _fsLangInfo   = lang_info
             , _fsContents   = BL.lines contents
             , _fsFilteredContents = []
             , _fsInBlockComment = False
             }

langInfoMap :: Map.Map String LangInfo
langInfoMap = Map.fromList [ ("c",  cLangInfo      )
                           , ("hs", haskellLangInfo)
                           ]

-- sloch target
--
-- Counts the SLOC in |target| and prints to stdout.
--
sloch :: FilePath -> IO ()
sloch target = do
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
         {-mapM_ (putStrLn . BL.unpack) sloc-}
         putStrLn $ target ++ ": " ++ show (length sloc) ++ " lines"
      Nothing -> return ()

slochDirectory :: FilePath -> IO ()
slochDirectory target = do
   contents <- getDirectoryContents_ target
   let full_path_contents = map (target </>) contents
   forM_ full_path_contents sloch

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
         maybe_line <- removeLineComment (lazyToStrictBS x) >>=
                       removeBlockComment . lazyToStrictBS  >>=
                       removeBoilerPlate . bStrip    -- strip before matching boiler plate

         fsContents .= xs    -- TODO: This vs. %= tail

         when (isJust maybe_line) $
            fsFilteredContents %= (fromJust maybe_line :)  -- TODO: fromJust -_-

         filterContents_

-- removeLineComment line
--
-- Removes line comment from |line|, if any.
--
removeLineComment :: B.ByteString -> Sloch BL.ByteString
removeLineComment line = do
   line_comment <- liftM lazyToStrictBS $ use (fsLangInfo . liLineComment)
   let (line', _) = B.breakSubstring line_comment line
   return $ BL.fromChunks [line']

-- removeBlockComment line
--
-- Remove block comment from |line|, if any. Modifies state per entering
-- or leaving a block comment.
--
-- Currently this function is not very smart:
--
--  - First, it possibly exits block comment state by if there exists an
--    end-comment without a preceding begin-comment.
--
--  - Then, it will enter block comment state if it finds a begin-comment
--    and no matching end-comment, but either way, will delete from the
--    begin-comment onward.
--
-- Thus, it can get confused in many ways, such as:
--
-- int foo = 1; /* comment */ /* another!
--
-- which will result in the block comment state NOT being entered, due to
-- the matching end-comment (and deletion of the entire line from
-- begin-comment onward).
--
-- Because all of these little corner cases all look like horrible style to
-- me, and this simplistic approach is much faster than a mini parser would
-- be, I'm sticking to it. Please let me know if there is a good argument
-- against the implementation described above, as I would love to improve
-- this code :)
--
removeBlockComment :: B.ByteString -> Sloch BL.ByteString
removeBlockComment line = do
   line' <- possiblyEndBlockComment line >>= possiblyEnterBlockComment
   return $ BL.fromChunks [line']

-- getBlockComment
--
-- Get the block comment of the current language as a strict byte string
-- tuple.
--
getBlockComment :: Sloch (B.ByteString, B.ByteString)
getBlockComment = liftM (over both lazyToStrictBS) $ use (fsLangInfo . liBlockComment)

-- possiblyEndBlockComment line
--
-- If there exists an end-comment without a begin-comment before it in
-- |line|, delete up to and including the end-comment, leave the in block
-- comment state, and return the remaining line.
--
possiblyEndBlockComment :: B.ByteString -> Sloch B.ByteString
possiblyEndBlockComment line = do
   (begin_c, end_c) <- getBlockComment

   let (before_end, end_on)   = B.breakSubstring end_c   line
   let (_,          begin_on) = B.breakSubstring begin_c before_end

   if (not . B.null) end_on && B.null begin_on
      then do
         fsInBlockComment .= False
         return $ B.drop (B.length end_c) end_on
      else return line

-- possiblyEnterBlockComment line
--
-- If there exists a begin-comment in |line|, delete everything from it
-- onward. Additionally, if there is no end-comment after it, enter the
-- in block comment state.
--
possiblyEnterBlockComment :: B.ByteString -> Sloch B.ByteString
possiblyEnterBlockComment line = do
   (begin_c, end_c) <- getBlockComment

   let (before_begin, begin_on) = B.breakSubstring begin_c line
   let (_,            end_on)   = B.breakSubstring end_c   begin_on

   if B.null begin_on
      then return line
      else do
         when (B.null end_on) $
            fsInBlockComment .= True

         return before_begin

-- removeBoilerPlate line
--
-- Returns Nothing if the line is boiler plate, or Just |line| if it isn't.
-- Requires |line| to be stripped.
--
removeBoilerPlate :: BL.ByteString -> Sloch (Maybe BL.ByteString)
removeBoilerPlate line =
   -- Hacky unintuitive spot for filtering null lines. This is better than
   -- having "null" explicitly be boiler plate in each language info, and
   -- works here because |line| is stripped, but there is probably a better
   -- spot for it.
   if BL.null line
      then return Nothing
      else do
         line_filters <- use $ fsLangInfo . liBoilerPlate

         return $
            if or $ line_filters <*> pure line
               then Nothing
               else Just line

---------------------------------------------------------------------------

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
   sloch target
