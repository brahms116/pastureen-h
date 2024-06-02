module Util (unformatJsonStr) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLU

unformatJsonStr :: BLU.ByteString -> BLU.ByteString
unformatJsonStr s = case decode s :: Maybe Value of
  Just v -> encode v
  Nothing -> error "Failed to parse JSON"
