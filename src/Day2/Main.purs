module Day2.Main where 

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

runA :: Effect Unit 
runA = 
    readTextFile UTF8 "./src/Day2/input.txt"
        <#> lines 
        <#> A.filter (not S.null)
        <#> map (words >>> map (I.fromString >>> fromMaybe 0))
        <#> map isReportSafe
        <#> A.filter ((==) true)
        <#> A.length
        <#> show
        >>= log


isReportSafe :: Array Int -> Boolean 
isReportSafe levels = 
    levels 
        # pairs 
        # map (\(T.Tuple a b) -> (a - b))
        # (A.all (\x -> between 1 3 x) || A.all (\x -> between (-3) (-1) x))

pairs :: forall a. Array a -> Array (T.Tuple a a)
pairs xs = A.zip xs (fromMaybe [] $ A.tail xs)
