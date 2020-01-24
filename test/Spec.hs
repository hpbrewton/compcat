import Lib

import qualified Data.Map as M
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Control.Monad


helper = do 
    let treeSchema = M.fromList [(0, []), (1, [1, 1, 0])]
    let dat = M.fromList [(0, M.fromList []), (1, M.fromList [ (43, [Left 44, Left 45, Right "hello"]), (44, [Left 0, Left 0, Right "three"]), (45, [Left 46, Left 47, Right "four"]), (46, [Left 0, Left 0, Right "five"]), (47, [Left 0, Left 0, Right "six"])])]
    let db = Database treeSchema dat
    kconvolution db 1 4 (Left 43)

kconv1 :: Bool
kconv1 = helper == Just ["","","","","","","three","","","five","","","six","four","hello"]
    where 

tests = [ testGroup "KConv" [testProperty "kconv1" kconv1]]

main = do 
    putStrLn $ show helper
    defaultMain tests
