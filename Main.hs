module Main where

import Text.CSV (parseCSVFromFile, Field)
import Graphics.Gloss (display, Display(..), white, Picture(..))
import Data.Map (fromList, Map, (!))

main :: IO ()
main = do
  nods <- nodes
  edgs <- edges
  display (InWindow "Map Plotter" (400, 400) (10, 10)) white (graph nods edgs)
  return ()
  -- drawGraph nods edges
  where graph n e = Pictures $ map (\(_, (n1, n2)) -> Line [n ! n1, n ! n2]) e

nodes :: IO (Map String (Float, Float))
nodes = parse "nodes.csv" parseNode >>=  return . fromList
  where
    parseNode (nodeID:lat:lon:xs) = (nodeID, (asFloat lat, asFloat lon))
    parseNode _ = ("", (0,0))
    asFloat s = (read s) :: Float

edges :: IO [(String, (String, String))]
edges = parse "edges.csv" parseEdge where
  parseEdge (edgeID:node1:node2:xs) = (edgeID, (node1, node2))
  parseEdge _ = ("", ("",""))

parse filename f = do
  csvData <- parseCSVFromFile filename
  case csvData of 
    Left err -> print err >> return []
    Right values -> return $ map f (tail values)

