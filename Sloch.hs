{-# LANGUAGE MultiWayIf, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module Main where

import Debug.Trace

import FilePathUtils
import Language
import MapUtils

import Control.Applicative ((<*>), pure)
import Control.Lens ((.=), (%=), (^.), _2, foldrOf, makeLenses, set, traverse, use)
import Control.Exception (throw, try)
import Control.Monad (forM, forM_, liftM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, execState)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.List (sortBy)
import Data.Maybe (fromJust, isJust)
import Data.Ord (comparing)
import GHC.IO.Exception (IOErrorType(..))
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hPutStr, stderr)
import System.IO.Error (ioeGetErrorType)
import System.Posix.Files (fileExist, getSymbolicLinkStatus, isDirectory, isRegularFile)
import Text.Printf (hPrintf, printf)

import qualified Data.Text    as T
import qualified Data.Text.IO as T   -- Strictly read files to close handles
import qualified Data.Map     as Map

version :: String
version = "0.1"

versionStr :: String
versionStr = "Sloch v" ++ version ++ " by Mitchell Rosen"

type LineCountInfo = (FilePath, Int)
type Sloch = WriterT [LineCountInfo] IO

-- The state of a file being filtered, including what language it's in.
--
data FileState =
   FileState { _fsLanguage         :: Language       -- The language of the file.
             , _fsContents         :: [T.Text] -- The remaining unfiltered lines.
             , _fsFilteredContents :: [T.Text] -- The lines filtered thus far.
             , _fsInBlockComment   :: Bool           -- Whether or not we are inside a block comment.
             }

makeLenses ''FileState

-- initFileState contents
--
-- Creates a FileState from a file as a ByteString.
--
initFileState :: Language -> T.Text -> FileState
initFileState lang_info contents =
   FileState { _fsLanguage         = lang_info
             , _fsContents         = T.lines contents
             , _fsFilteredContents = []
             , _fsInBlockComment   = False
             }

-- sloch target
--
-- Counts the SLOC in |target| and prints to stdout.
--
sloch :: FilePath -> Sloch ()
sloch target = do
   file_exists <- liftIO $ fileExist target

   if file_exists
      then do
         file_status <- liftIO $ getSymbolicLinkStatus target

         if | isRegularFile file_status -> slochFile      target
            | isDirectory   file_status -> slochDirectory target
            | otherwise                 -> return ()
      else liftIO $ hPutStr stderr (printf "File '%s' does not exist\n" target)

-- slochFile target
--
-- Counts the SLOC in |target| and prints to stdout.
--
slochFile :: FilePath -> Sloch ()
slochFile target = do
   let ext = takeExtension_ target

   case Map.lookup ext langInfoMap of
      Just lang_info -> do

         result <- liftIO $ try (T.readFile target)
         case result of
            Right contents -> do
               let sloc = filterContents lang_info contents
               {-liftIO $ mapM_ (putStrLn . T.unpack) sloc -- DEBUG: print lines to console-}
               tell [(target, length sloc)]
            Left err -> case ioeGetErrorType err of
                           InvalidArgument -> liftIO $ hPrintf stderr "%s -- check your character encoding\n" (show err)
                           _               -> throw err
      Nothing -> return ()

-- slochDirectory target
--
-- Counts the SLOC in every file inside |target|, including subdirectories.
--
slochDirectory :: FilePath -> Sloch ()
slochDirectory target = do
   contents <- liftIO $ getDirectoryContents_ target
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
filterContents :: Language -> T.Text -> [T.Text]
filterContents lang_info contents = final_file_state ^. fsFilteredContents
   where
      init_file_state :: FileState
      init_file_state = initFileState lang_info contents

      final_file_state :: FileState
      final_file_state = execState filterContents_ init_file_state

filterContents_ :: State FileState ()
filterContents_ = do
   file_contents <- use fsContents
   case file_contents of
      []     -> return ()
      (x:xs) -> do
         fsContents .= xs    -- TODO: This vs. %= tail

         -- First, see if we escape out of a block comment, saving off the
         -- remaining line (to further reduce).
         line <- possiblyEndBlockComment x

         in_block_comment <- use fsInBlockComment

         unless in_block_comment $ do
            maybe_line <- removeBlockComment line >>=
                          removeLineComment       >>=
                          removeBoilerPlate . T.strip -- strip before matching boiler plate

            -- FIXME: This can probably be rewritten to not use isJust/fromJust
            when (isJust maybe_line) $
               fsFilteredContents %= (fromJust maybe_line :)

         filterContents_

-- removeLineComment line
--
-- Removes line comment from |line|, if any.
--
removeLineComment :: T.Text -> State FileState T.Text
removeLineComment line = do
   line_comment <- use (fsLanguage . lLineComment)
   let (line', _) = T.breakOn line_comment line
   return line'

-- removeBlockComment line
--
-- Remove block comment from |line|, if any, possibly entering block state
-- if a begin-block but no end-block is found.
--
-- Currently this function is not very smart. It will enter block comment
-- state if it finds a begin-comment and no matching end-comment, but
-- either way, will delete from the begin-comment onward.
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
removeBlockComment :: T.Text -> State FileState T.Text
removeBlockComment line = do
   maybe_block_comments <- use $ fsLanguage . lBlockComment
   case maybe_block_comments of
      Just (begin_c, end_c) -> do
         let (before_begin, begin_on) = T.breakOn begin_c line
         let (_,            end_on)   = T.breakOn end_c   begin_on

         if T.null begin_on
            then return line
            else do
               when (T.null end_on) $
                  fsInBlockComment .= True

               return before_begin
      Nothing -> return line

-- possiblyEndBlockComment line
--
-- If there exists an end-comment without a begin-comment before it in
-- |line|, delete up to and including the end-comment, leave the in block
-- comment state, and return the remaining line.
--
possiblyEndBlockComment :: T.Text -> State FileState T.Text
possiblyEndBlockComment line = do
   in_block_comment <- use fsInBlockComment

   if in_block_comment
      then do
         -- FIXME: fromJust -_-
         -- We can (safely) assume that if fsInBlockComment was set, then
         -- this language has a block comment that was entered. Still,
         -- fromJust...
         (begin_c, end_c) <- liftM fromJust $ use (fsLanguage . lBlockComment)

         let (before_end, end_on)   = T.breakOn end_c   line
         let (_,          begin_on) = T.breakOn begin_c before_end

         if (not . T.null) end_on && T.null begin_on
            then do
               fsInBlockComment .= False
               return $ T.drop (T.length end_c) end_on
            else return line
      else return line

-- removeBoilerPlate line
--
-- Returns Nothing if the line is boiler plate, or Just |line| if it isn't.
-- Requires |line| to be stripped.
--
removeBoilerPlate :: T.Text -> State FileState (Maybe T.Text)
removeBoilerPlate line =
   -- Hacky unintuitive spot for filtering null lines. This is better than
   -- having "null" explicitly be boiler plate in each language info, and
   -- works here because |line| is stripped, but there is probably a better
   -- spot for it.
   if T.null line
      then return Nothing
      else do
         line_filters <- use $ fsLanguage . lBoilerPlate

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
usage = "Usage: sloch (filename|dirname)+" -- ++ concatMap show options

parseOptions :: [String] -> IO (Options, [String])
parseOptions args =
   case getOpt Permute options args of
      (opt_mods, non_opts, []) -> return (makeOptions opt_mods, non_opts)
      (_, _, errors) -> error $ concat errors ++ usage

-- makeOptions opt_mods
--
-- Applies each of |opt_mods| to the default options and return it.
--
makeOptions :: [(Options -> Options)] -> Options
makeOptions = foldl (flip id) defaultOptions

totalLineCounts :: [LineCountInfo] -> Map.Map T.Text Int
totalLineCounts = foldr totalLineCounts_ Map.empty
   where totalLineCounts_ :: LineCountInfo -> Map.Map T.Text Int -> Map.Map T.Text Int
         totalLineCounts_ (file_name, line_count) acc =
            let language = fromJust $ Map.lookup (takeExtension_ file_name) langInfoMap -- TODO: fromJust -_-
            in adjustWithDefault line_count (+ line_count) (language ^. lName) acc

main :: IO ()
main = do
   (opts, targets) <- getArgs >>= parseOptions

   when (opts ^. optShowHelp)    $ putStrLn usage
   when (opts ^. optShowVersion) $ putStrLn versionStr

   results <- forM targets (runWriterT . sloch)

   let counts        = concatMap snd results                                               -- List of line counts, one per file
   let aggr_counts   = sortBy (flip $ comparing snd) (Map.toList $ totalLineCounts counts) -- Aggregated by filetypes
   let total_counts  = foldrOf (traverse . _2) (+) 0 aggr_counts                           -- Sum of all lines in all files

   when (opts ^. optVerbose) $ do
      forM_ counts $ \(s, n) -> putStr $ printf "%s: %d lines\n" s n
      putStrLn "----------"

   forM_ aggr_counts $ \(s, n) -> putStr $
      printf "Total %s: %d lines (%.2f%%)\n" (T.unpack s) n (100 * (fromIntegral n :: Float) / (fromIntegral total_counts :: Float))
