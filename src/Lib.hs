module Lib
    ( someFunc
    ) where

import qualified Data.Map as M
import Control.Monad

type Data = String
type Id = Int
type FK = Int -- foreign key
type TypeSchema = M.Map Id [Id]
type Table = M.Map FK [Either FK Data]

data Database = Database {
    tables :: M.Map Id Table
}

-- down window
window :: TypeSchema -> Int -> Id -> Maybe [Id]
window schema 0     t = return []
window schema depth t = do 
    children <- t `M.lookup` schema 
    if 0 == length children
        then Just [t]
        else fmap join $ sequence $ fmap (window schema (depth-1)) children
        
-- groethendiek construction
-- groeth :: TypeSchema -> M.Map

someFunc :: IO ()
someFunc = do 
    let schema = M.fromList $ [(0, []), (1, [1, 1, 0])]
    let wind = window schema 5 1
    putStrLn $ show wind