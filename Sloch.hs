{-# LANGUAGE MultiWayIf, OverloadedStrings, TemplateHaskell #-}

module Main where

import Debug.Trace

import FilePathUtils
import Language
import MapUtils

import Control.Applicative ((<*>), pure)
import Control.Lens ((.=), (%=), (^.), _2, foldrOf, makeLenses, set, traverse, use)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (fromJust, isJust)
import Data.String.Utils (strip)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf (printf)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as Map

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
             , _fsContents         :: [B.ByteString] -- The remaining unfiltered lines.
             , _fsFilteredContents :: [B.ByteString] -- The lines filtered thus far.
             , _fsInBlockComment   :: Bool           -- Whether or not we are inside a block comment.
             }

makeLenses ''FileState

-- initFileState contents
--
-- Creates a FileState from a file as a ByteString.
--
initFileState :: Language -> B.ByteString -> FileState
initFileState lang_info contents =
   FileState { _fsLanguage         = lang_info
             , _fsContents         = B.lines contents
             , _fsFilteredContents = []
             , _fsInBlockComment   = False
             }

-- sloch target
--
-- Counts the SLOC in |target| and prints to stdout.
--
sloch :: FilePath -> Sloch ()
sloch target = do
   file_exists      <- liftIO $ doesFileExist      target
   directory_exists <- liftIO $ doesDirectoryExist target

   if | file_exists      -> slochFile target
      | directory_exists -> slochDirectory target
      | otherwise        -> liftIO $ putStrLn $ "'" ++ target ++ "' does not exist"

-- slochFile target
--
-- Counts the SLOC in |target| and prints to stdout.
--
slochFile :: FilePath -> Sloch ()
slochFile target = do
   let ext = takeExtension_ target

   case Map.lookup ext langInfoMap of
      Just lang_info -> do
         contents <- liftIO $ B.readFile target -- Strict read to close file handle
         let sloc = filterContents lang_info contents
         {-liftIO $ mapM_ (putStrLn . B.unpack) sloc -- DEBUG: print lines to console-}
         tell [(target, length sloc)]
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
filterContents :: Language -> B.ByteString -> [B.ByteString]
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
      [] -> return ()
      (x:xs) -> do
         fsContents .= xs    -- TODO: This vs. %= tail

         -- First, see if we escape out of a block comment, saving off the
         -- remaining line (to further reduce).
         line <- possiblyEndBlockComment x

         in_block_comment <- use fsInBlockComment

         unless in_block_comment $ do
            maybe_line <- removeBlockComment line >>=
                          removeLineComment       >>=
                          removeBoilerPlate . strip' -- strip before matching boiler plate

            when (isJust maybe_line) $
               fsFilteredContents %= (fromJust maybe_line :)  -- TODO: fromJust -_-

         filterContents_

   where strip' = B.pack . strip . B.unpack

-- removeLineComment line
--
-- Removes line comment from |line|, if any.
--
removeLineComment :: B.ByteString -> State FileState B.ByteString
removeLineComment line = do
   line_comment <- use (fsLanguage . lLineComment)
   let (line', _) = B.breakSubstring line_comment line
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
removeBlockComment :: B.ByteString -> State FileState B.ByteString
removeBlockComment line = do
   (begin_c, end_c) <- use $ fsLanguage . lBlockComment

   let (before_begin, begin_on) = B.breakSubstring begin_c line
   let (_,            end_on)   = B.breakSubstring end_c   begin_on

   if B.null begin_on
      then return line
      else do
         when (B.null end_on) $
            fsInBlockComment .= True

         return before_begin

-- possiblyEndBlockComment line
--
-- If there exists an end-comment without a begin-comment before it in
-- |line|, delete up to and including the end-comment, leave the in block
-- comment state, and return the remaining line.
--
possiblyEndBlockComment :: B.ByteString -> State FileState B.ByteString
possiblyEndBlockComment line = do
   in_block_comment <- use fsInBlockComment

   if in_block_comment
      then do
         (begin_c, end_c) <- use $ fsLanguage . lBlockComment

         let (before_end, end_on)   = B.breakSubstring end_c   line
         let (_,          begin_on) = B.breakSubstring begin_c before_end

         if (not . B.null) end_on && B.null begin_on
            then do
               fsInBlockComment .= False
               return $ B.drop (B.length end_c) end_on
            else return line
      else return line

-- removeBoilerPlate line
--
-- Returns Nothing if the line is boiler plate, or Just |line| if it isn't.
-- Requires |line| to be stripped.
--
removeBoilerPlate :: B.ByteString -> State FileState (Maybe B.ByteString)
removeBoilerPlate line =
   -- Hacky unintuitive spot for filtering null lines. This is better than
   -- having "null" explicitly be boiler plate in each language info, and
   -- works here because |line| is stripped, but there is probably a better
   -- spot for it.
   if B.null line
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

totalLineCounts :: [LineCountInfo] -> Map.Map B.ByteString Int
totalLineCounts = foldr totalLineCounts_ Map.empty
   where totalLineCounts_ :: LineCountInfo -> Map.Map B.ByteString Int -> Map.Map B.ByteString Int
         totalLineCounts_ (file_name, line_count) acc =
            let language = fromJust $ Map.lookup (takeExtension_ file_name) langInfoMap -- TODO: fromJust -_-
            in adjustWithDefault line_count (+ line_count) (language ^. lName) acc

main :: IO ()
main = do
   (opts, targets) <- getArgs >>= parseOptions

   when (opts ^. optShowHelp)    $ putStrLn usage
   when (opts ^. optShowVersion) $ putStrLn versionStr

   results <- forM targets (runWriterT . sloch)

   let counts        = concatMap snd results                     -- List of line counts, one per file
   let aggr_counts   = Map.toList $ totalLineCounts counts       -- Aggregated by filetypes
   let total_counts  = foldrOf (traverse . _2) (+) 0 aggr_counts -- Sum of all lines in all files

   when (opts ^. optVerbose) $ do
      forM_ counts $ \(s, n) -> putStr $ printf "%s: %d lines\n" s n
      putStrLn "----------"

   forM_ aggr_counts $ \(s, n) -> putStr $
      printf "Total %s: %d lines (%.2f%%)\n" (B.unpack s) n (100 * (fromIntegral n :: Float) / (fromIntegral total_counts :: Float))
