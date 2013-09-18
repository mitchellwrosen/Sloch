module Options 
    ( Options(..)
    , parseOptions
    ) where

import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt)

data Options = Options
    { optDepth           :: Int
    , optIncludeDotfiles :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optDepth           = 0
    , optIncludeDotfiles = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['d'] ["depth"]            (ReqArg (\d o -> o { optDepth = (read d) }) "depth DEPTH") "set depth"
    , Option []    ["include-dotfiles"] (NoArg $ \o -> o { optIncludeDotfiles = True } )           "include dotfiles"
    ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions args =
   case getOpt Permute options args of
      (opt_mods, non_opts, []) -> return (makeOptions opt_mods, non_opts)
      (_, _, errors)           -> error $ concat errors ++ usage

makeOptions :: [(Options -> Options)] -> Options
makeOptions = foldr id defaultOptions

usage :: String
usage = unlines
    [ "Usage: sloch dirname [options]"
    , "   -d INT, --depth INT      Set depth of traversal (default 0)."
    , "   --include-dotfiles       Include dotfiles."
    ]
