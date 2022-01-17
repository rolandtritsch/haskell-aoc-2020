-- |
-- Various utility functions.
module Util where

import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)

-- | Get the (raw) input (as one line (with newlines in it)).
inputRaw :: String -> String
inputRaw fileName = unsafePerformIO $ readFile fileName

-- | Nice(er) trace function.
trace' :: Show a => String -> a -> a
-- trace' _prefix what = what
trace' prefix what = trace text what
  where
    text = prefix ++ (show what)
