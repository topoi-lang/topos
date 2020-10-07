-- |

module Module where

import Data.HashSet (HashSet)
import qualified Name
import qualified Parsing.Span as Span

data Header = Header { _exposedNames :: !ExposedNames
                     , _imports :: [Import]
                     } deriving (Eq, Show)


data ExposedNames = Exposed (HashSet Name.Pre)
                  | AllExposed
                  deriving (Eq, Show)


data Import = Import { _span :: Span.Absolute
                     , _module :: Name.Module
                     , _alias :: (Span.Absolute, Name.Pre)
                     , _importedNames :: !ExposedNames
                     } deriving (Eq, Show)
