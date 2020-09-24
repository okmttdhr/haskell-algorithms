import Data.Map (Map)
import qualified Data.Map as Map

-- Map.lookup "Foo" hashTable
hashTable :: Map String Integer
hashTable = Map.fromList [("Foo", 1), ("Bar", 2), ("Buz", 3)]
