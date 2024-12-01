module Day1.Main where 

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Data.Array as A
import Data.String as S
import Data.String.Utils (lines, words)
import Data.Int as I
import Data.Maybe (fromMaybe)
import Data.Tuple as T
import Data.Number (abs, fromString)
import Data.Filterable (filterMap)
import Data.Foldable (sum)

type DualList = T.Tuple (Array Int) (Array Int)

runA :: Effect Unit 
runA = 
    loadLists
        <#> findDifferences
        <#> sum 
        <#> show
        >>= log

runB :: Effect Unit
runB = 
    loadLists
        <#> findSimilarities
        <#> sum
        <#> show
        >>= log

loadLists :: Effect DualList
loadLists = 
    readTextFile UTF8 "./src/Day1/input.txt"
        <#> lines 
        <#> A.filter (not S.null)
        <#> map (words >>> filterMap I.fromString)
        <#> A.foldr buildLists (T.Tuple [] []) -- Fold into Tuple of sorted lists

buildLists :: (Array Int) -> DualList -> DualList
buildLists row (T.Tuple xs ys) = 
    case row of 
        [x, y] -> T.Tuple (A.insertBy compare x xs) (A.insertBy compare y ys)
        _ -> T.Tuple xs ys

-- For each pair, find the numerical difference
findDifferences :: DualList -> Array Number 
findDifferences (T.Tuple xs ys) = 
    A.zip xs ys 
        # map (\(T.Tuple x y) -> abs ((I.toNumber x) - (I.toNumber y)))


-- For each number in the first list, count how many times it appears in the second list and multiply the count by its own value
findSimilarities :: DualList -> Array Number
findSimilarities (T.Tuple xs ys) =
    let 
        scoreSimilarity :: (Array Int) -> Int -> Number
        scoreSimilarity bs a =
            A.filter ((==) a) bs
                # A.length
                # (*) a
                # I.toNumber
    in
        map (scoreSimilarity ys) xs



