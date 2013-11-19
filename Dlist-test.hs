module Main where

import Criterion
import Criterion.Main
import Data.DList (DList, toList, fromList)
import qualified Data.DList as DL

-- two 'main' functions, which change how the lists are evaluated/forced.
-- results are essentially similar.
-- main = defaultMain
main = force `seq` snocList `seq` defaultMain
  [ bench "list length" $ whnf baselineList defData
  , bench "dlist roundtrip" $ whnf dlistrtrip defData
  , bench "dlist 10"        $ whnf (dlistRep 10      ) def100k
  , bench "dlist 1000"      $ whnf (dlistRep 1000    ) def1k
  , bench "dlist 100000"    $ whnf (dlistRep 100000  ) def10
  , bench "dlist 10000000"  $ whnf (dlistRep 1000000 ) [1]
  , bench "dlist snocList"  $ whnf (length . toList)   snocList
  ]
  where
    force = sum $ map length [defData, def100k, def1k, def10]

defData = [1..1000000:: Int]
def100k = [1..100000 :: Int]
def1k   = [1..1000   :: Int]
def10   = [1..10     :: Int]
snocList  = foldl DL.snoc DL.empty defData

baselineList :: [a] -> Int
baselineList = length

dlistrtrip :: [a] -> Int
dlistrtrip = length . toList . fromList

dlistRep :: Int -> [a] -> Int
dlistRep n xs = length . toList $ DL.concat $ replicate n (fromList xs)
