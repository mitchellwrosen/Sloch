{- Command line interface for Sloch. Exports a datatype consisting of all command line options/arguments, and a function
 - to retrieve it in IO.
 - -}
module Cli
    ( Cli(..)
    , OptGit
    , OptIncludeDotfiles
    , OptVerbose
    , parseCli
    ) where

import Control.Applicative
import Data.Monoid
import Options.Applicative
    ( InfoMod, Parser, ParserInfo
    , argument, customExecParser, failureCode, help, helper, info, long, metavar, option, prefs
    , short, showDefault, showHelpOnError, str, switch, value
    )
import Options.Applicative.Types (ParserPrefs) -- Options.Applicative, y u no export this?

type OptIncludeDotfiles = Bool
type OptGit             = Bool
type OptVerbose         = Bool

data Cli = Cli
    { cliDepth           :: Int
    , cliIncludeDotfiles :: OptIncludeDotfiles
    , cliGit             :: OptGit
    , cliVerbose         :: OptVerbose
    , cliDir             :: FilePath
    }

parseCli :: IO Cli
parseCli = customExecParser pprefs parseCli'
  where
    pprefs :: ParserPrefs
    pprefs = prefs showHelpOnError

parseCli' :: ParserInfo Cli
parseCli' = info (helper <*> cli) infoMod
  where
    cli :: Parser Cli
    cli = pure Cli <*> depth <*> includeDotfiles <*> git <*> verbose <*> dir

    depth :: Parser Int
    depth = option $
         value   0
      <> short   'd'
      <> long    "depth"
      <> help    "Set the depth under DIR to summarize the counts at."
      <> metavar "DEPTH"
      <> showDefault

    includeDotfiles :: Parser OptIncludeDotfiles
    includeDotfiles = switch $
         long "include-dotfiles"
      <> help "Include dotfiles"

    git :: Parser OptGit
    git = switch $
         short 'g'
      <> long  "git"
      <> help  "Sloc this Git repository (ignores DIR). May be used anywhere inside the repository."

    verbose :: Parser OptVerbose
    verbose = switch $
         short 'v'
      <> long "verbose"
      <> help "Print output verbosely."

    dir :: Parser FilePath
    dir = argument str $
         value   "."
      <> metavar "DIR"
      <> help    "Directory to sloc. Ignored if --git is provided."
      <> showDefault

    infoMod :: InfoMod Cli
    infoMod = failureCode (-1)
