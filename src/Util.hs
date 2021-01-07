-- |
-- Various utility functions.
module Util where

import System.IO.Unsafe (unsafePerformIO)

-- | get the (raw) input (as one line (with newlines in it))
inputRaw :: String -> String
inputRaw fileName = unsafePerformIO $ readFile fileName
