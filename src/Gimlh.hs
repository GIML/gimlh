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
-- * Functions for parse and modify GIML
  parseString
, parseFile
, simplifyGiml
)

where

import Prelude
import System.IO
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Numeric (readFloat)
import Data.Char (isDigit)

data GimlVal
    = Text String    -- ^ Text value
    | List [String]  -- ^ List value
    | Number Integer -- ^ Integer number value
    | Float Double   -- ^ Float number value
    deriving (Show)

data GimlType
    = TextG   -- ^ Type for text value, stored in 'GimlNode'
    | ListG   -- ^ Type for list value, stored in 'GimlNode'
    | NumberG -- ^ Type for integer number value, stored in 'GimlNode'
    | FloatG  -- ^ Type for float number value, stored in 'GimlNode'
    deriving (Show)

type GimlNode = (String, GimlType, GimlVal)

type SimpleGiml = [(String, GimlVal)]

type Giml = [GimlNode]

-- |The 'parseFile' method will parse 'GIML' from file by 'FilePath'
parseFile :: FilePath -> IO Giml
parseFile path = do
    contents <- readFile path
    return $ parseString contents

-- |The 'parseString' method will parse 'GIML' from pure string.
parseString :: String -> Giml
parseString contents = parseLines (lines contents) Nothing

-- |The 'simplifyGiml' method will remove types from 'Giml' creating
-- 'SimpleGiml' object
simplifyGiml :: Giml -> SimpleGiml
simplifyGiml giml = map (\(a, b, c) -> (a, c)) giml

-- |The 'parseLines' method takes list of pure strings and initial
-- 'GimlNode' and recursively parses them into 'Giml'
parseLines :: [String] -> Maybe GimlNode -> Giml
parseLines [] Nothing = []
parseLines [] (Just node) = [node]
parseLines (line:rest) node = case parseLine line node of
                                (Nothing, Nothing) -> parseLines rest node
                                (Nothing, newNode) -> parseLines rest newNode
                                (oldNode, newNode) -> (fromJust oldNode) : parseLines rest newNode

-- |The 'parseLine' method takes string and node and try to recognize that
-- it should be attached to value in original node or create new node. Or
-- skip the line if it is comment or current node is 'Nothing'
parseLine :: String -> Maybe GimlNode -> (Maybe GimlNode, Maybe GimlNode)
parseLine ('#':_) _               = (Nothing, Nothing)
parseLine line@(':':_) (Just old) = (return old, return $ newNode (words line))
parseLine line@(':':_) Nothing    = (Nothing, return $ newNode (words line))
parseLine line (Just var)         = (Nothing, return $ setNode var line)
parseLine _ Nothing               = (Nothing, Nothing)

-- |The 'newNode' receive two strings as type and name of node and creates
-- new node according to type
newNode :: [String] -> GimlNode
newNode (":list:":varName)  = (varName !! 0, ListG, List [])
newNode (":vlist:":varName) = (varName !! 0, ListG, List [])
newNode (":text:":varName)  = (varName !! 0, TextG, Text [])
newNode (":num:":varName)   = (varName !! 0, NumberG, Number 0)

-- |The 'setNode' method receive node and value and attach value to value
-- in current node
setNode :: GimlNode -> String -> GimlNode
setNode (varName, ListG, xs) "" = (varName, ListG, xs)
setNode (varName, ListG, xs) x  = case (words x) !! 0 of
                                  "-"       -> (varName, ListG, List $ (val2List xs) ++ [(unwords $ tail (words x))])
                                  otherwise -> (varName, ListG, List $ (val2List xs) ++ (splitOn ", " x))
setNode (varName, TextG, xs) x  = (varName, TextG, Text $ (val2Text xs) ++ x ++ "\n")
setNode (varName, _, val) ""    = (varName, NumberG, val)
setNode (varName, _, _) newVal  = let parsedNum = fromJust $ parseNum newVal
                                    in
                                      case parsedNum of
                                        (Number val) -> (varName, NumberG, Number val)
                                        (Float val)  -> (varName, FloatG, Float val)
                                        otherwise    -> (varName, NumberG, Number 0)

-- |The 'val2Text' method gets pure string from 'GimlVal'
val2Text (Text val) = val
-- |The 'val2List' method gets pure list from 'GimlVal'
val2List (List val) = val

-- |The 'parseNum' method gets integer or float number from numeric
-- 'GimlVal'
parseNum :: String -> Maybe GimlVal
parseNum str = do
    let digitsAndDot = filter (\x -> isDigit x || x == '.') str
    if any (\x -> x == '.') digitsAndDot then
      return $ Float $ fst (readFloat digitsAndDot !! 0)
    else
      return $ Number $ read digitsAndDot
