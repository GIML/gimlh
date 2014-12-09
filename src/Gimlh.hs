{-|
Module        : Gimlh
Description   : Module for parsing GIML from file or string
Copyright     : (c) Alexey Gaziev, 2014
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Haskell parser for GIML.
-}
module Gimlh
(
-- * Data types
  Giml(..)
, GimlVal(..)
, GimlType(..)
, GimlNode(..)
, SimpleGiml(..)

-- * Functions for parse and modify GIML
, parseString
, parseFile
, simplifyGiml
, fetch
, fetchG
, val2Str
, val2List
)

where

import Prelude
import System.IO
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Numeric (readFloat)
import Data.Char (isDigit)

-- | Value represent parsed data for specified variable name
data GimlVal
    = Text String    -- ^ Text value
    | List [String]  -- ^ List value
    | Number Integer -- ^ Integer number value
    | Float Double   -- ^ Float number value
    deriving (Show)

-- | Type of value for internal functions
data GimlType
    = TextG   -- ^ Type for text value, stored in 'GimlNode'
    | ListG   -- ^ Type for list value, stored in 'GimlNode'
    | NumberG -- ^ Type for integer number value, stored in 'GimlNode'
    | FloatG  -- ^ Type for float number value, stored in 'GimlNode'
    deriving (Show)

-- | Type 'GimlNode' represent list of tuples. Tuple contains 'String' as key,
-- 'GimlType' as type of value and 'GimlVal' as value
type GimlNode = (String, GimlType, GimlVal)

-- | Type 'SimpleGiml' represent list of tuples. Tuple contains 'String' as
-- key and 'GimlVal' as value
type SimpleGiml = [(String, GimlVal)]

-- | Type 'Giml' represent list of 'GimlNode's
type Giml = [GimlNode]

-- | The 'parseFile' method will parse 'GIML' from file by 'FilePath'
parseFile :: FilePath -> IO Giml
parseFile path = do
    contents <- readFile path
    return $ parseString contents

-- | The 'parseString' method will parse 'GIML' from pure string.
parseString :: String -> Giml
parseString contents = parseLines (lines contents) Nothing

-- | The 'simplifyGiml' method will remove types from 'Giml' creating
-- 'SimpleGiml' object
simplifyGiml :: Giml -> SimpleGiml
simplifyGiml = map (\(a, b, c) -> (a, c))

-- | The 'fetch' method will fetch values from simplified giml
-- by given key
fetch :: SimpleGiml -> String -> Maybe GimlVal
fetch [] _ = Nothing
fetch ((key, val):xs) req = if key == req
                              then return val
                              else fetch xs req

-- | The 'fetchG' method will fetch values from giml
-- by given key
fetchG :: Giml -> String -> Maybe GimlVal
fetchG giml key = fetch (simplifyGiml giml) key

-- | The 'val2Str' method will retrun values stored in GIML in string
-- representation
val2Str :: GimlVal -> String
val2Str (Text val)   = val
val2Str (List val)   = show val
val2Str (Number val) = show val
val2Str (Float val)  = show val

-- | The 'val2List' method will retrun values stored in GIML in list of string
-- representation
val2List :: GimlVal -> [String]
val2List (List val)   = val
val2List (Text val)   = [val]
val2List (Number val) = [show val]
val2List (Float val)  = [show val]

-- The 'parseLines' method takes list of pure strings and initial
-- 'GimlNode' and recursively parses them into 'Giml'
parseLines :: [String] -> Maybe GimlNode -> Giml
parseLines [] Nothing = []
parseLines [] (Just node) = [node]
parseLines (line:rest) node = case parseLine line node of
                                (Nothing, Nothing) -> parseLines rest node
                                (Nothing, newNode) -> parseLines rest newNode
                                (oldNode, newNode) -> fromJust oldNode : parseLines rest newNode

-- The 'parseLine' method takes string and node and try to recognize that
-- it should be attached to value in original node or create new node. Or
-- skip the line if it is comment or current node is 'Nothing'
parseLine :: String -> Maybe GimlNode -> (Maybe GimlNode, Maybe GimlNode)
parseLine ('#':_) _               = (Nothing, Nothing)
parseLine line@(':':_) (Just old) = (return old, return $ newNode (words line))
parseLine line@(':':_) Nothing    = (Nothing, return $ newNode (words line))
parseLine line (Just var)         = (Nothing, return $ setNode var line)
parseLine _ Nothing               = (Nothing, Nothing)

-- The 'newNode' receive two strings as type and name of node and creates
-- new node according to type
newNode :: [String] -> GimlNode
newNode (":list:":key)  = (head key, ListG, List [])
newNode (":vlist:":key) = (head key, ListG, List [])
newNode (":text:":key)  = (head key, TextG, Text [])
newNode (":num:":key)   = (head key, NumberG, Number 0)

-- The 'setNode' method receive node and value and attach value to value
-- in current node
setNode :: GimlNode -> String -> GimlNode
setNode orig@(key, ListG, xs) "" = orig
setNode (key, ListG, xs) x       = case head $ words x of
                                    "-"       -> (key, ListG, List $ val2List xs ++ [unwords . tail $ words x])
                                    otherwise -> (key, ListG, List $ val2List xs ++ splitOn ", " (removeCommaAtEnd x))
setNode orig@(key, TextG, xs) "" = case xs of
                                     Text ""   -> orig
                                     otherwise -> (key, TextG, Text $ val2Text xs ++ "\n")
setNode (key, TextG, xs) x       = (key, TextG, Text $ val2Text xs ++ x ++ "\n")
setNode (key, _, val) ""         = (key, NumberG, val)
setNode (key, _, _) newVal       = let parsedNum = fromJust $ parseNum newVal
                                     in
                                       case parsedNum of
                                         (Number val) -> (key, NumberG, Number val)
                                         (Float val)  -> (key, FloatG, Float val)
                                         otherwise    -> (key, NumberG, Number 0)

-- The 'val2Text' method gets pure string from 'GimlVal'
val2Text (Text val) = val
-- The 'val2List' method gets pure list from 'GimlVal'
val2List (List val) = val
-- The 'val2List' method gets pure list from 'GimlVal'
val2Int  (Number val) = val
-- The 'val2List' method gets pure list from 'GimlVal'
val2Dbl  (Float val) = val

removeCommaAtEnd :: String -> String
removeCommaAtEnd str = if last str == ',' && last (init str) /= '\\'
                         then init str
                         else str

-- The 'parseNum' method gets integer or float number from numeric
-- 'GimlVal'
parseNum :: String -> Maybe GimlVal
parseNum str = do
    let digitsAndDot = filter (\x -> isDigit x || x == '.') str
    if '.' `elem` digitsAndDot
      then return $ Float $ fst . head $ readFloat digitsAndDot
      else return $ Number $ read digitsAndDot
