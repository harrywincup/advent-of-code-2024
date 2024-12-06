module Day5.Main where 

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Data.Array as A
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.String.Utils (lines)
import Data.Int as I
import Data.Foldable (sum)
import Data.Maybe (fromMaybe)

type Pages = Array Int
type Rule = Array Int

runA :: Effect Unit 
runA = do
    input <- readTextFile UTF8 "./src/Day5/input.txt"

    let ls = input # lines # A.drop 1

    let rules = ls 
            # A.takeWhile (SCU.contains $ S.Pattern "|") 
            # map (S.split (S.Pattern "|") >>> map (I.fromString >>> fromMaybe 0))

    let updates = ls
            # A.reverse 
            # A.drop 1
            # A.takeWhile (SCU.contains $ S.Pattern ",") 
            # map (S.split (S.Pattern ",") >>> map (I.fromString >>> fromMaybe 0))

    let a = map (checkAgainstAll rules) updates

    log $ show (sum a)

checkAgainstAll :: Array Rule -> Pages -> Int
checkAgainstAll rules pages =
    let 
        valid = rules  
            # map (checkAgainstOne pages)
            # A.all ((==) true)

        i = pages 
                # A.length 
                # I.toNumber 
                # (*) 0.5
                # I.floor

        n = case valid of 
                true -> A.index pages i # fromMaybe 0
                false -> 0
   in
       n

checkAgainstOne :: Pages -> Rule -> Boolean 
checkAgainstOne pages rule = do
    case A.intersect rule pages of 
        [l, r] -> (A.elemIndex l pages) < (A.elemIndex r pages)
        _ -> true
