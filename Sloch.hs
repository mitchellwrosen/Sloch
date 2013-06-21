{-# LANGUAGE TemplateHaskell #-}

module Main where

import Debug.Trace

import LangInfo

import Control.Lens ((.~), both, element, makeLenses, over, set)
import Control.Monad (forM_, when)
import Data.List (intercalate)
import Data.String.Utils (strip)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.Exit (ExitCode(..), exitWith)
import Text.Regex.PCRE (MatchLength, MatchOffset, (=~), getAllMatches)

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as F

-- POSIX regex escape characters.
--
regexEscapeChars :: String
regexEscapeChars = ".^$*+?()[{\\"

langInfoMap :: Map.Map String LangInfo
langInfoMap = Map.fromList [ ("c"      , cLangInfo      )
                           , ("haskell", haskellLangInfo)
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
         contents <- readFile target
         let sloc = filterContents lang_info contents
         mapM_ putStrLn sloc
         putStrLn $ "Lines: " ++ show (length sloc)
      Nothing -> do
         putStrLn $ "Unknown file extension: '" ++ ext ++ "'"
         exitWith $ ExitFailure 1

slochDirectory :: FilePath -> IO ()
slochDirectory target = do
   contents <- getDirectoryContents' target
   forM_ contents slouch

-- getDirectoryContents' target
--
-- Gets directory contents from |target|, less "." and ".."
--
getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' target = do
   contents <- getDirectoryContents target
   return $ filter (`notElem` [".", ".."]) contents

-- filterContents lang_info contents
--
-- Filters comments out of |contents| (a flat string) per |lang_info|,
-- returning the filtered contents as a list of strings. This function
-- basically reads backwards.
--
filterContents :: LangInfo -> String -> [String]
filterContents lang_info =
   removeBoilerPlate lang_info . -- Remove boiler plate
   removeEmptyLines            . -- Removes lines consisting of only whitespace
   lines                       . -- Split on newlines
   replaceComments   lang_info   -- Replace comments with newlines

-- replaceComments lang_info contents
--
-- Replaces all comments in |contents| with newline characters, where the
-- comment syntax is defined in |lang_info|.
--
replaceComments :: LangInfo -> String -> String
replaceComments lang_info contents =
   let
      lineCommentIndices  = findIndices (line_comment ++ ".*\n")                     contents
      blockCommentIndices = findIndices (begin_comment ++ "(.|\\n)*" ++ end_comment) contents -- multi-line regex hack
      indices             = Set.union lineCommentIndices blockCommentIndices
   in
      setAll '\n' contents indices

   where
      line_comment                 =           regexEscape $ lineComment lang_info
      (begin_comment, end_comment) = over both regexEscape $ blockComment lang_info

-- findIndices pattern contents
--
-- Returns all indices in |contents| that were part of some instance of
-- |pattern| (a regex)
findIndices :: String -> String -> Set.Set Int
findIndices pattern haystack =
   let offsets = getAllMatches $ haystack =~ pattern :: [(MatchOffset, MatchLength)]
   in offsetsToIndices offsets

-- offsetsToIndices matches
--
-- Converts |offsets| into a set of indices.
--
-- For example:
--    [(1,2), (5,4)] to [[1,2], [5,6,7,8]] to Set(1,2,5,6,7,8)
--
offsetsToIndices :: [(MatchOffset, MatchLength)] -> Set.Set Int
offsetsToIndices = foldr insertIndices Set.empty
   where
      insertIndices :: (MatchOffset, MatchLength) -> Set.Set Int -> Set.Set Int
      insertIndices (offset, len) set = insertAll set $ take len [offset..]

      insertAll :: Ord a => Set.Set a -> [a] -> Set.Set a
      insertAll = foldr Set.insert

-- setAll x xs indices
--
-- Sets the |indices| elements of |xs| to |x|.
--
setAll :: F.Foldable t => a -> [a] -> t Int -> [a]
setAll x = F.foldr (\index xs -> element index .~ x $ xs)

regexEscape :: String -> String
regexEscape = concatMap (\c -> if c `elem` regexEscapeChars
                                  then '\\':[c]
                                  else [c]
                        )

-- removeBoilerPlate lang_info contents
--
-- Removes boiler plate lines from |contents| per the boiler plate matching
-- rules in |lang_info|.
--
removeBoilerPlate :: LangInfo -> [String] -> [String]
removeBoilerPlate lang_info contents =
   foldr applyFilter contents (boilerPlate lang_info)

   where
      applyFilter :: LineFilter -> [String] -> [String]
      applyFilter f = filter (not . f)

-- removeEmptyLines contents
--
-- Removes empty lines from |contents|, where "empty" means containing only
-- whitespace.
--
removeEmptyLines :: [String] -> [String]
removeEmptyLines = filter (not . null) . map strip

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

