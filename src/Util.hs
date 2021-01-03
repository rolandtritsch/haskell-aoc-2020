{- |
Various utility functions.
-}
module Util where

import           Data.Void
import           System.IO.Unsafe

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = P.Parsec Void String

-- | get the (raw) input (as one line (with newlines in it))
inputRaw :: String -> String
inputRaw fileName = unsafePerformIO $ readFile fileName

-- | get the (parsed) input.
inputParser :: Parser a -> String -> a
inputParser parser fileName = unsafePerformIO $ inputParser' parser fileName

inputParser' :: Parser a -> String -> IO a
inputParser' parser fileName = do
        contents <- readFile fileName
        case P.parse parser fileName contents of
                Left  e -> error $ P.errorBundlePretty e
                Right a -> return a

-- | parse integer(s).
integer :: Parser Int
integer = L.decimal

signedInteger :: Parser Int
signedInteger = L.signed (return ()) L.decimal
