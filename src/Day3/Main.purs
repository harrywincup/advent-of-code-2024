module Day3.Main where 

import Prelude

import Debug (spy)
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Data.Array as A
import Data.String as S
import Data.String.Utils (lines)
import Data.Tuple as T
import Data.Int as I
import Data.Function as F
import Parsing (Parser, runParser, ParseError)
import Parsing.String as PS
import Parsing.Combinators as PC
import Parsing.Combinators.Array as PCA
import Parsing.String.Basic as PSB
import Control.Lazy (defer)
import Data.Either (Either(..), fromRight)
import Data.Foldable (sum)

runA :: Effect Unit 
runA = 
    readTextFile UTF8 "./src/Day3/test-input.txt"
        <#> lines 
        <#> A.concatMap ((F.flip runParser $ parseMul) >>> (fromRight []))
        <#> spy "what"
        <#> sum
        <#> show
        >>= log

parseMul :: Parser String (Array Number)
parseMul = do 
    result <- PCA.many $ PC.try parseValid `PC.(<|>)` skipInvalid
    pure $ spy "hmm" result

parseValid :: Parser String Number
parseValid = do
    _ <- PS.char '('
    a <- PSB.number
    _ <- PS.char ','
    b <- PSB.number
    _ <- PS.char ')'

    pure $ a * b

skipInvalid :: Parser String Number
skipInvalid = do
    _ <- PC.optional (PS.string "do" *> PC.notFollowedBy (PS.string "n't") *> PS.string "()")
    _ <- PS.anyTill $ PS.string "mul"
    r <- parseValid
    pure $ spy "aa" r

--parseMul :: Parser String (Array Number)
--parseMul = do 
--    result <- (PCA.many parseValid) `PC.(<|>)` (defer \_ -> skipInvalid)
--    pure $ spy "hmm" result
--
--parseValid :: Parser String Number
--parseValid = do
--    _ <- PS.char '('
--    a <- PSB.number
--    _ <- PS.char ','
--    b <- PSB.number
--    _ <- PS.char ')'
--
--    pure $ a * b
--
--skipInvalid :: Parser String (Array Number)
--skipInvalid = do
--    _ <- PC.optional (PS.string "do" *> PC.notFollowedBy (PS.string "n't") *> PS.string "()")
--    PCA.many ((PS.anyTill $ PS.string "mul") *> parseValid)
--
