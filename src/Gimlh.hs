module Gimlh
(
  parseString
, parseFile
)

where

import Prelude
import System.IO
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Numeric (readFloat)
import Data.Char (isDigit)

data GimlVal = Text String
           | List [String]
           | Number Integer
           | Float Double
           deriving (Show)

data GimlType = TextG | ListG | NumberG | FloatG deriving (Show)

type GimlNode = (String, GimlType, GimlVal)

type Giml = [GimlNode]

parseFile :: FilePath -> IO Giml
parseFile path = do
    contents <- readFile path
    return $ parseString contents

parseString :: String -> Giml
parseString contents = parseLines (lines contents) Nothing

parseLines :: [String] -> Maybe GimlNode -> Giml
parseLines [] Nothing = []
parseLines [] (Just node) = [node]
parseLines (line:rest) node = case parseLine line node of
                                (Nothing, Nothing) -> parseLines rest node
                                (Nothing, newNode) -> parseLines rest newNode
                                (oldNode, newNode) -> (fromJust oldNode) : parseLines rest newNode

parseLine :: String -> Maybe GimlNode -> (Maybe GimlNode, Maybe GimlNode)
parseLine ('#':_) _               = (Nothing, Nothing)
parseLine line@(':':_) (Just old) = (return old, return $ newNode (words line))
parseLine line@(':':_) Nothing    = (Nothing, return $ newNode (words line))
parseLine line (Just var)         = (Nothing, return $ setNode var line)
parseLine _ Nothing               = (Nothing, Nothing)

newNode :: [String] -> GimlNode
newNode (":list:":varName)  = (varName !! 0, ListG, List [])
newNode (":vlist:":varName) = (varName !! 0, ListG, List [])
newNode (":text:":varName)  = (varName !! 0, TextG, Text [])
newNode (":num:":varName)   = (varName !! 0, NumberG, Number 0)

setNode :: GimlNode -> String -> GimlNode
setNode (varName, ListG, xs) "" = (varName, ListG, xs)
setNode (varName, ListG, xs) x = case (words x) !! 0 of
                                  "-"       -> (varName, ListG, List $ (val2List xs) ++ [(unwords $ tail (words x))])
                                  otherwise -> (varName, ListG, List $ (val2List xs) ++ (splitOn ", " x))
setNode (varName, TextG, xs) x = (varName, TextG, Text $ (val2Text xs) ++ x ++ "\n")
setNode (varName, _, val) "" = (varName, NumberG, val)
setNode (varName, _, _) newVal = let parsedNum = fromJust $ parseNum newVal
                                 in
                                   case parsedNum of
                                     (Number val) -> (varName, NumberG, Number val)
                                     (Float val)  -> (varName, FloatG, Float val)
                                     otherwise    -> (varName, NumberG, Number 0)

val2Text (Text val) = val
val2List (List val) = val

parseNum :: String -> Maybe GimlVal
parseNum str = do
    let digitsAndDot = filter (\x -> isDigit x || x == '.') str
    if any (\x -> x == '.') digitsAndDot then
      return $ Float $ fst (readFloat digitsAndDot !! 0)
    else
      return $ Number $ read digitsAndDot
