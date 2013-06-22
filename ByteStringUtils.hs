module ByteStringUtils where

import Data.String.Utils (strip)

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as BL

bIsInfixOf :: BL.ByteString -> BL.ByteString -> Bool
bIsInfixOf needle haystack = any (BL.isPrefixOf needle) (BL.tails haystack)

bStrip :: BL.ByteString -> BL.ByteString
bStrip = BL.pack . strip . BL.unpack -- TODO: roll own strip

lazyToStrictBS :: BL.ByteString -> B.ByteString
lazyToStrictBS = B.concat . BL.toChunks
