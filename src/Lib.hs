module Lib
    ( 
        Database (..),
        Data,
        kconvolution
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
kconvolution db _ 0  _   = Just [] -- if it is a foreign key, and we're done,don't do anything
kconvolution db _ _ (Right d)  = Just [d] -- if it is just data, use it
kconvolution db i k (Left key) = do 
    schema <- i `M.lookup` (schema db) -- get schema of type
    table <- i `M.lookup` (tables db) -- get table for type
    case (key `M.lookup` table) of 
        Just v -> fmap join $ sequence $ zipWith (\i' e' -> kconvolution db i' (k-1) e') schema v
        Nothing -> do
            sz <- size db i k -- note, this does not go down because we pass off to size
            Just $ take sz $ repeat ""
    where 
        size :: Database -> Id -> Int -> Maybe Int 
        size db i k = do 
            entry <- i `M.lookup` (schema db) 
            if null entry 
                then Just 1 -- that is it is a final type 
                else (if k /= 0 then fmap sum $ sequence $ map (\i' -> size db i' (k-1)) entry else Just 0)

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

