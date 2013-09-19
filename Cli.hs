module Cli
    ( Cli(..)
    , parseCli
    ) where

import Options.Applicative
    ( InfoMod, Parser, ParserInfo
    , (<>), (<$>), (<*>), argument, customExecParser, failureCode, help, helper, info, long, metavar, option, prefs
    , short, showDefault, showHelpOnError, str, switch, value
    )
import Options.Applicative.Types (ParserPrefs) -- Options.Applicative, y u no export this?

data Cli = Cli
    { cliDepth :: Int
    , cliIncludeDotfiles :: Bool
    , cliGit :: Bool
    , cliDir :: FilePath
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
    cli = Cli <$> depth <*> includeDotfiles <*> git <*> dir

    depth :: Parser Int
    depth = option $
        value   0
     <> short   'd'
     <> long    "depth"
     <> help    "Set the depth under DIR to summarize the counts at."
     <> metavar "DEPTH"
     <> showDefault

    includeDotfiles :: Parser Bool
    includeDotfiles = switch $
        long "include-dotfiles"
     <> help "Include dotfiles"

    git :: Parser Bool
    git = switch $
        short 'g'
     <> long  "git"
     <> help  "Sloc this Git repository (ignores DIR). May be used anywhere inside the repository."

    dir :: Parser FilePath
    dir = argument str $
        value   "."
     <> metavar "DIR"
     <> help    "Directory to sloc. Ignored if --git is provided."
     <> showDefault

    infoMod :: InfoMod Cli
    infoMod = failureCode (-1)
