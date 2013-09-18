{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module LineCounter (countLines) where

import Control.Monad (forever, unless)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Lens ((^.), (.=), (%=), makeLenses, use)
import Data.Char (isSpace)
import Pipes
import Pipes.Lift (execStateP)
import Pipes.Safe (runSafeT)
import Pipes.Safe.Prelude (readFile)
import Prelude hiding (readFile, map)

import qualified Pipes.Prelude as P

import Language

data LineCount = LineCount
    { _lineCount :: Int
    , _inComment :: Bool
    } deriving Show

initLineCount :: LineCount
initLineCount = LineCount
    { _lineCount = 0
    , _inComment = False
    }

makeLenses ''LineCount

countLines :: FilePath -> Language -> IO Int
countLines file_path lang = do
    line_count <-
        runSafeT $
            runEffect $
                execStateP initLineCount $
                    readFile file_path                  >->
                    P.map trim                          >->
                    P.filter (not . null)               >->
                    P.filter (not . isLineComment lang) >->
                    hoist (hoist lift) (countLinesConsumer lang)
    return $ line_count ^. lineCount

countLinesConsumer :: Language -> Consumer' String (StateT LineCount IO) ()
countLinesConsumer lang = forever $ await >>= count lang

count :: Language -> String -> Consumer' String (StateT LineCount IO) ()
count lang line
    -- isEnd must come before isBegin, so that inline block comments function correctly.
    -- Otherwise, there must be an "if isEnd" inside the "isBegin" body.
    | isEndBlockComment   lang line = inComment .= False
    | isBeginBlockComment lang line = inComment .= True
    | otherwise = unlessM (use inComment) $ lineCount %= (+1)

trim :: String -> String
trim = dropWhile isSpace

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb action = mb >>= flip unless action
