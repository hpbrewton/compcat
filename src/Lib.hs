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
    schema :: TypeSchema,
    tables :: M.Map Id Table
}

-- given a table and a primary kfey find the depth k convolution
-- right now this does not take into account possible commutativity that exists in the the DB
-- for example consider basic image convolution
kconvolution :: Database -> Id -> Int -> (Either FK Data) -> Maybe [Data]
kconvolution db _ _ (Right d)  = Just [d]
kconvolution db _ 0 (Left _)   = Just []
kconvolution db i k (Left key) = do 
    schema <- i `M.lookup` (schema db) 
    table <- i `M.lookup` (tables db) 
    etnry <- key `M.lookup` table
    fmap join $ sequence $ zipWith (\i' e' -> kconvolution db i' (k-1) e') schema etnry

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
{-
"hello" {
    "three"
    "four" {
        "five"
        "six"
    }
}

-}
someFunc :: IO ()
someFunc = do 
    let treeSchema = M.fromList [(0, []), (1, [1, 1, 0])]
    let dat = M.fromList [(0, M.fromList []), (1, M.fromList [ (43, [Left 44, Left 45, Right "hello"]), (44, [Left 44, Left 44, Right "three"]), (45, [Left 46, Left 47, Right "four"]), (46, [Left 46, Left 46, Right "five"]), (47, [Left 47, Left 47, Right "six"])])]
    let db = Database treeSchema dat
    let kc = kconvolution db 1 3 (Left 43)
    putStrLn $ show kc