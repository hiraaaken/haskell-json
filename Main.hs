module JSONParser where

import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Data.Bits (shiftL)
import Data.Char (isDigit, isHexDigit, isSpace, chr, ord, digitToInt)
import Data.Functor (($>))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Numeric (showHex)
import Test.QuickCheck hiding (Positive)

data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber { int :: Integer, frac :: [Int], exponent :: Integer}
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Generic)

instance Show JValue where
  show value = case value of
    JNull           -> "null"
    JBool True      -> "true"
    JBool False     -> "false"
    JString s       -> showJsonString s
    JNumber s [] 0  -> show s
    JNumber s f 0   -> show s ++ "." ++ concatMap show f
    JNumber s f e   -> show s ++ "." ++ concatMap show f ++ "e" ++ show e
    JArray a        -> "[" ++ intercalate ", " (map show a) ++ "]"
    JObject o       -> "{" ++ intercalate ", " (map showKV o) ++ "}"
      where
        showKV (k, v) = showJsonString k ++ ": " ++ show v


showJsonString :: String -> String
showJsonString s = "\"" ++ concatMap showJsonChar s ++ "\""

isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

showJsonChar :: Char -> String
showJsonChar c = case c of
  '\'' -> "''"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '/'  -> "\\/"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | isControl c -> "\\u" ++ showJSONNonASCIIChar c
  _ -> [c]
  where
    showJSONNonASCIIChar c =
      let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a

jNullGen :: Gen JValue
jNullGen = pure JNull

jBoolGen :: Gen JValue
jBoolGen = JBool <$> arbitrary

jNumberGen :: Gen JValue
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0, 9)) <*> arbitrary

jsonStringGen :: Gen String
jsonStringGen =
  concat <$> listOf (oneof [ vectorOf 1 arbitraryUnicodeChar
                           , escapedUnicodeChar ])
  where
    escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitLetters)
    hexDigitLetters    = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

jStringGen :: Gen JValue
jStringGen = JString <$> jsonStringGen

jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)

jObjectGen :: Int -> Gen JValue
jObjectGen = fmap JObject . scale (`div` 2) . listOf . objKV . (`div` 2)
  where
    objKV n = (,) <$> jsonStringGen <*> jValueGen n

jValueGen :: Int -> Gen JValue
jValueGen n = if n < 5
  then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
  else frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]
  where
    scalarGens = [ jNullGen
                 , jBoolGen
                 , jStringGen
                 , jNumberGen
                 ]
    compositeGens m = [ jArrayGen m
                      , jObjectGen m
                      ]

instance Arbitrary JValue where
  arbitrary = sized jValueGen
  shrink    = genericShrink

jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =
  scale (round . sqrt . fromIntegral)
  . listOf
  . elements
  $ [' ', '\n', '\r', '\t']

stringify :: JValue -> Gen String
stringify = pad . go
  where
    surround l r j = l ++ j ++ r
    pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
    commaSeparated = pad . pure . intercalate ","

    go value = case value of
      JArray elements -> 
        mapM (pad . stringify) elements
          >>= fmap (surround "[" "]") . commaSeparated
      JObject kvs ->
        mapM stringifyKV kvs >>= fmap (surround "{" "}") . commaSeparated
      _           -> return $ show value

    stringifyKV (k, v) = 
      surround <$> pad (pure $ showJsonString k) <*> stringify v <*> pure ":"


