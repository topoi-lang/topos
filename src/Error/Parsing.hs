-- |

module Error.Parsing where

import Data.ByteString (ByteString)
import qualified Parsing.Position as Position

data Parsing = Parsing { reason :: Maybe ByteString
                       , expected :: [ByteString]
                       , position :: Either EOF Position.Absolute
                       } deriving (Eq, Ord, Show)

data EOF = EOF
  deriving (Eq, Ord, Show)
