module Database where
import Data.Map (Map, empty)

type Database = Map String String

createDb :: Database
createDb = empty