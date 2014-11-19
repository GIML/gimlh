module Gimlh
(
  parseLines
--, parseFile
)

where

import Prelude
import System.IO
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data GimlVal = Text String
           | List [String]
           | Number Integer
           | Float Double
           | Empty
           deriving (Show)

data GimlType = TextG | ListG | NumberG | FloatG deriving (Show)

type GimlNode = (String, GimlType, GimlVal)

type Giml = [GimlNode]

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
newNode (":num:":varName)   = (varName !! 0, NumberG, Empty)

setNode :: GimlNode -> String -> GimlNode
setNode (varName, ListG, xs) "" = (varName, ListG, xs)
setNode (varName, ListG, xs) x = case (words x) !! 0 of
                                  "-"       -> (varName, ListG, List $ (val2List xs) ++ [(unwords $ tail (words x))])
                                  otherwise -> (varName, ListG, List $ (val2List xs) ++ (splitOn ", " x))
setNode (varName, TextG, xs) x = (varName, TextG, Text $ (val2Text xs) ++ x ++ "\n")
setNode (varName, NumberG, val) "" = (varName, NumberG, val)
setNode (varName, NumberG, _) newVal = (varName, NumberG, Number $ (read newVal :: Integer))

val2Text (Text val) = val
val2List (List val) = val
val2Num (Number val) = val
