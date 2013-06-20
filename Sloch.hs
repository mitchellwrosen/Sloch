module Main where

import Control.Monad (liftM, unless, when)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import Data.String.Utils (split, strip)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.Exit (ExitCode(..), exitWith)

import qualified Data.Map as Map

knownExtensions :: [String]
knownExtensions = ["c"]

langInfoMap :: Map.Map String LangInfo
langInfoMap = Map.fromList [ ("c"      , cLangInfo      )
                           , ("haskell", haskellLangInfo)
                           ]

printUsage :: IO ()
printUsage = print "Usage: sloch (filename | dirname)"

-- doExitFailure action code does |action| then exits with exit code |code|
doExitFailure :: IO a -> Int -> IO ()
doExitFailure action code = action >> exitWith (ExitFailure code)

main :: IO ()
main = do
   args <- getArgs

   when (length args /= 1) $
      doExitFailure printUsage 1

   let target = head args

   file_exists      <- doesFileExist target
   directory_exists <- doesDirectoryExist target

   -- Syntastic complains when I use MultiWayIf... sigh
   case () of
      _ | file_exists      -> slochFile target
      _ | directory_exists -> slochDirectory target
        | otherwise        -> error $ target ++ " does not exist"

-- slochFile target counts the SLOC in |target| and prints to stdout.
slochFile :: FilePath -> IO ()
slochFile target = do
   let ext = drop 1 $ takeExtension target -- drop '.'

   case Map.lookup ext langInfoMap of
      Just lang_info -> do
         contents <- liftM lines $ readFile target

         let sloc = filterContents lang_info contents
         {-let sloc = filter (not . isLineComment) . filter (not . null) . map trimWhitespace . lines $ contents-}
         print sloc
         print $ "Lines: " ++ show (length sloc)
      Nothing        -> doExitFailure (putStrLn $ "Unknown file extension: '" ++ ext ++ "'") 1

slochDirectory :: FilePath -> IO ()
slochDirectory = undefined

---------------------------------------------------------------------------

-- filterContents lang_info contents filters |contents| per |lang_info|,
-- returning the filtered contents.
filterContents :: LangInfo -> [String] -> [String]
filterContents lang_info =
   filter (not . null)                    . -- remove blank lines

                                            -- TODO: block comments
   map (head . split line_comment)        . -- remove end-of-line line comments
   filter (not . isPrefixOf line_comment) . -- remove standalone line comments
   map strip                                -- strip leading and trailing whitespace

   where line_comment = lineComment lang_info
         boiler_plate = boilerPlate lang_info


---------------------------------------------------------------------------

type LineFilter = String -> Bool
data LangInfo =
   LangInfo { lineComment  :: String           -- line comment sequence
            , blockComment :: (String, String) -- (begin block comment, end block comment)
            , boilerPlate  :: [LineFilter]     -- boiler plate lines that don't "count" as real code
            }

---------------------------------------------------------------------------

cLangInfo = LangInfo cLineComment cBlockComment cBoilerPlate
cLineComment  = "//"
cBlockComment = ("/*", "*/")
cBoilerPlate  = [ isPrefixOf "#include"
                , (==) "}"
                , (==) "{"
                ]

---------------------------------------------------------------------------

haskellLangInfo = LangInfo haskellLineComment haskellBlockComment haskellBoilerPlate
haskellLineComment = "--"
haskellBlockComment = ("{-", "-}")
haskellBoilerPlate = [ isPrefixOf "module " -- module declaration
                     , isPrefixOf "import " -- import statement
                     , isInfixOf  "::"      -- type annotations
                     ]
