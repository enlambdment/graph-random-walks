module Lib where

import Data.Graph 
import Data.Array 
import System.Random       
import System.IO.Unsafe    (unsafeInterleaveIO, unsafePerformIO)
import Control.Monad       (liftM2, join)
import Control.Applicative (liftA2)



-- Given a graph       gr :: Graph ~ Array Vertex [Vertex]
-- and a vertex        v  :: Vertex
-- use System.Random, Data.Array functionality to specify 
-- a random walk (possibly non-terminating) within the graph.

-- Check if (v :: Vertex) is contained in (gr :: Array Vertex [Vertex]).
vertexInGraph :: Vertex -> Graph -> Bool
vertexInGraph v gr =
  -- v `elem` [(fst $ bounds gr) .. (snd $ bounds gr)]
  -- INSTEAD:
  -- range :: Ix a => (a, a) -> [a]
  v `elem` range (bounds gr)

-- Check if (v :: Vertex) has no edge pointing to any other vertex.
noNextVertex :: Vertex -> Graph -> Bool
noNextVertex v gr = null $ gr ! v 

-- Given a (gr :: Array Vertex [Vertex]) and (v :: Vertex),
-- get a random next vertex, in IO (fails if (v :: Vertex)
-- has no edge pointing to any other vertex.)
randNextVertIO :: Graph -> Vertex -> Maybe (IO Vertex)
randNextVertIO gr v 
    | noNextVertex v gr = Nothing
    | otherwise         =
            let     nextVerts  = gr ! v                                  :: [Vertex]
                    randIdxIO  = randomRIO (0, length nextVerts - 1)     :: IO Int
            in      Just $ randIdxIO >>= (return . (nextVerts !!))       :: Maybe (IO Vertex)

---------------------------------------

-- Here's code for evolving a (possibly infinite) 
-- random walk from an input graph & starting vertex.
-- 'makeRandomWalk' works as expected, never returning a list
-- where a vertex is followed by another vertex not accessible
-- from the former vertex.

-- Given a (gr :: Array Vertex [Vertex]) and (v0 :: Vertex)
-- a starting vertex, produce a (possibly infinite) list of 
-- Vertex'es representing a random walk through 'gr', in IO.
getRandomWalk :: Graph -> Vertex -> IO [Vertex]
getRandomWalk gr v0 = case randNextVertIO gr v0 of
    Nothing             -> return []
    Just iov            -> do 
      v     <- iov 
      vs    <- getRandomWalk gr v
      return $ v : vs

-- For use with graphs that give rise to infinite random walks
-- in the absence of any external "instruction" to terminate.
-- 'unsafeInterleaveIO' is worked into the body of 'getRandomWalk'.
-- L for Lazy
getRandomWalkL :: Graph -> Vertex -> IO [Vertex]
getRandomWalkL gr v0 = case randNextVertIO gr v0 of
    Nothing             -> return []
    Just iov            -> do 
      v     <- iov 
      vs    <- unsafeInterleaveIO $ getRandomWalkL gr v 
      return $ v:vs

-- The higher-order version of makeRandomWalk. For use with
-- input (fn :: Graph -> Vertex -> IO [Vertex]) so that I can
-- give functions with different laziness / strictness behavior
-- as input
-- (e.g. doRandomWalk getRandomWalk == makeRandomWalk)
doRandomWalk :: (Graph -> Vertex -> IO [Vertex])
            ->   Graph -> Vertex -> IO [Vertex]
doRandomWalk fn gr v0
  | not (vertexInGraph v0 gr)         = return []
  | otherwise                         = 
     liftM2 (:) (return v0) (fn gr v0)

makeRandomWalk, makeRandomWalkL :: Graph 
                                -> Vertex 
                                -> IO [Vertex]
makeRandomWalk  = doRandomWalk getRandomWalk
makeRandomWalkL = doRandomWalk getRandomWalkL

----------------------------------------------------------------

-- an example
type Node = Char
type Key  = Int

getFstOf3 :: (a, b, c) -> a
getFstOf3 (x, y, z) = x

myEdges :: [(Node, Key, [Key])]
myEdges = 
 [ ( 'c', 3, [4,5] ),
   ( 'd', 4, [3,5] ),
   ( 'e', 5, []    ) ]

myGraph :: Graph
myGraph = getFstOf3 $ graphFromEdges myEdges

myEdges2 :: [(Node, Key, [Key])]
myEdges2 =
  let cs = ['a'..'g']
  in  [ (cs !! j, j, [(j-2) `mod` 7, 
                      (j+2) `mod` 7]) | j <- [0..6] ]
myGraph2 :: Graph
myGraph2 = getFstOf3 $ graphFromEdges myEdges2
{-
  array (0,6) [ (0,[5,2]),(1,[6,3]),
                (2,[0,4]),(3,[1,5]),
                (4,[2,6]),(5,[3,0]),
                (6,[4,1])           ]
-}

