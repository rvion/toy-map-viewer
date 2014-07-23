{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Graphics.Rasterific (renderDrawing, withTexture, stroke, Join(..), Cap(..), line, V2(..))
import Graphics.Gloss (display, Display(..), white, Picture(..))
import Graphics.Rasterific.Texture (uniformTexture)
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Data.Map (fromList, Map, (!))
import Control.Monad (liftM2, forM_)
import Text.CSV (parseCSVFromFile)
import GHC.Float (float2Double)
import Data.List (foldl1')

type Point = (Float, Float)
type Segment = (Point,Point)
type BoundingBox = (Point,Point)
type TripPath = [Point]
type UID = String
type Nodes = [(UID, (Float, Float))]
type Edges = [(UID, (UID, UID))]

main :: IO ()
main = do
  nodes <- parseNodes "nodes.csv"
  edges <- parseEdges "edges.csv"
  let graph = buildSegments nodes edges
      bbox  = boundingBox nodes
  getLine >>= \x -> case x of 
    "i"   -> displayInteractive graph
    "s"   -> renderAsPng bbox graph (3000, 3000) "map.png"
    _     -> print "Invalid command" 
  
-- STATIC DISPLAY (save image file)
renderAsPng :: BoundingBox -> [Segment] -> (Float,Float) -> FilePath -> IO ()
renderAsPng ((xMin,yMin),(xMax,yMax)) s (width,height) fileName = writePng fileName $ draw $ do
  drawSegments s blueColor 1
  drawSegments testTripData redColor 5
  where --colors:
        white = PixelRGBA8 255 255 255 255
        blueColor = PixelRGBA8 0 0x86 0xc1 255
        redColor = PixelRGBA8 0xFF 0x53 0x73 255
        --drawing helpers
        draw = renderDrawing (round width) (round height) white
        drawSegments seg col w =  withTexture (uniformTexture col) $ forM_ seg $ \x -> toLine w (resize x)
        toLine w ((a,b),(c,d)) = stroke w JoinRound (CapRound, CapRound) $ line (V2 a b) (V2 c d)
        --size helpers
        vRatio = width  / (yMax - yMin)
        hRatio = height / (xMax - xMin)
        resize ((a,b),(c,d)) =  ( (hRatio * (a-xMin), vRatio * (b-yMin)), (hRatio * (c-xMin), vRatio * (d-yMin)))

boundingBox :: Nodes -> (Point, Point)
boundingBox nodes = ((xMin,yMin),(xMax,yMax))
  where xMin = minimum $ filter (/= 0) $ map (fst . snd) nodes
        yMin = minimum $ filter (/= 0) $ map (snd . snd) nodes
        xMax = maximum $ filter (/= 0) $ map (fst . snd) nodes
        yMax = maximum $ filter (/= 0) $ map (snd . snd) nodes

-- INTERACTIVE DISPLAY
displayInteractive :: [Segment] -> IO ()
displayInteractive s = display windowSetup white graph
  where windowSetup = (InWindow "Map Plotter" (400, 400) (10, 10))
        graph = Pictures $ map (Line . toPath) s
        toPath (p1,p2) = [p1,p2]

testTripData :: [Segment]
testTripData = zipWith (,) (init tripPath) (tail tripPath)
  where tripPath = [ (2.37393,48.844717), (2.4035178,48.8613619), (2.37367,48.844288), (2.387773,48.854485)]

-- PARSING
buildSegments :: Nodes -> Edges -> [Segment]
buildSegments n e = pureSegments (fromList n) e
  where pureSegments nodes edges = map toSegment edges
          where toSegment (_, (n1, n2)) = (nodes ! n1, nodes ! n2)

parseNodes :: FilePath -> IO Nodes
parseNodes fileName = parse fileName parseNode
  where parseNode (nodeID:lat:lon:xs) = (nodeID, (asFloat lat, asFloat lon))
        parseNode _ = ("", (0,0))
        asFloat s = (read s) :: Float

parseEdges :: FilePath -> IO Edges
parseEdges fileName = parse fileName parseEdge 
  where parseEdge (edgeID:node1:node2:xs) = (edgeID, (node1, node2))
        parseEdge _ = ("", ("",""))

parse :: FilePath -> ([String] -> a) -> IO [a]
parse filename f = do
  csvData <- parseCSVFromFile filename
  case csvData of 
    Left err -> print err >> return []
    Right values -> return $ map f (tail values)
