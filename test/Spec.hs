{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}

import Lib 
import Test.QuickCheck
import Data.Graph 
import Data.Array 
import System.Random       
import System.IO.Unsafe    (unsafeInterleaveIO, unsafePerformIO)
import Control.Monad       (liftM2, liftM3, join)
import Control.Applicative (liftA2)

-- Let's write two properties to test:
--  1. one for use with graphs whose random walks
-- terminate somewhere,
--  2. one for use with graphs whose random walks
-- may never terminate (e.g. graphs that are
-- strongly connected)
-- (knowing which one to use is left at the 
--  discretion of the user - not 'automating'
--  this for now.)

-- If random walks are always given back in IO,
-- and you can't (safely) access a value contained in IO
--  (except using     unsafePerformIO :: IO a -> a)
-- then how can I write this so that the output type
-- isn't 'IO Bool'? (not usable for property testing)

-- 1.

-- ASSUMPTION: 
--    v `vertexInGraph` gr
prop_TerminatingRandomWalk_WellFormed :: Graph 
                                      -> Vertex 
                                      -> Bool 
prop_TerminatingRandomWalk_WellFormed gr v =  
  -- unsafely obtain the final computed random walk
  let walk  = unsafePerformIO $ makeRandomWalk gr v 
      len_w = length walk 
  in 
  -- 0. short-circuit in case that 'not (v `vertexInGraph` gr)'
      ((not $ v `vertexInGraph` gr) && len_w == 0) ||
  -- 1. element 0 should be the input (v :: Vertex)
      ((v == walk !! 0) &&
  -- 2. element (j+1) should have an edge *to* it
  --    *from* element j, along graph (gr :: Graph)
      (and [ (walk !! (j + 1)) `elem` 
             (gr ! (walk !! j))     | j <- [0..(len_w - 2)] ]))

-- 2. 

-- ASSUMPTION: 
--    v `vertexInGraph` gr
prop_NonterminatingRandomWalk_WellFormed :: Graph 
                                         -> Vertex
                                         -> Bool 
prop_NonterminatingRandomWalk_WellFormed gr v =
  -- use makeRandomWalkL, so that only the elements needed
  -- can be taken (without forcing computation of the entire walk)
  let infWalk  = makeRandomWalkL gr v    -- :: IO [Vertex]
      cent     = (take 100) <$> infWalk  -- :: IO [Vertex]   
      walk100  = unsafePerformIO cent    -- ::    [Vertex] -- Is this sound?    
  in 
  -- 0. short-circuit in case that 'not (v `vertexInGraph` gr)'
      ((not $ v `vertexInGraph` gr) && length walk100 == 0) ||
  -- 1. element 0 should be the input (v :: Vertex)
      ((v == walk100 !! 0) &&
  -- 2. element (j+1) should have an edge *to* it
  --    *from* element j, along graph (gr :: Graph).
  --    For arbitrary (gr :: Graph), (v :: Vertex),
  --    the resulting random walk *may in fact terminate.*
  --    So be sure that j doesn't range over indexes that are too large. 
      (and [ (walk100 !! (j + 1)) `elem` 
             (gr ! (walk100 !! j))     
                | j <- [0..((min (length walk100) 100) - 2)] ]))

-- Enforces the requirement that we should have 
--    v `vertexInGraph` gr
vertInGraph_TerminatingRandomWalk_WellFormed, 
  vertInGraph_NonterminatingRandomWalk_WellFormed :: Graph 
                                                  -> Vertex 
                                                  -> Property  
vertInGraph_TerminatingRandomWalk_WellFormed gr v =
  not (v `vertexInGraph` gr) ==>
    prop_TerminatingRandomWalk_WellFormed gr v
vertInGraph_NonterminatingRandomWalk_WellFormed gr v =
  not (v `vertexInGraph` gr) ==>
    prop_NonterminatingRandomWalk_WellFormed gr v


-- Here's a way to get an arbitrary subset of some 
-- closed range of index values 
--    (a1, a2)          a1, a2 :: Ix a => a
-- which leverages
--    range :: Ix a => (a, a) -> [a]
genSubrange :: Ix a => (a, a) -> Gen [a]
genSubrange (j0, jf) = 
  let fullRange     = range (j0, jf)
      fullPairs     = [ (j, oneof $ return <$> [True, False]) | j <- fullRange ]
      genListPairs  = sequenceA $ sequenceA <$> fullPairs
      genListTrues  = (filter snd) <$> genListPairs
  in  (fst <$>) <$> genListTrues

-- Takes an input (n :: Int) to control the size of 
-- the arbitrary graph to be generated, then generates
-- a graph with *up to* n many vertices (but possibly fewer.)
n_genGraph :: Int -> Gen Graph -- Graph ~ Array Vertex [Vertex]
n_genGraph n = do
  v_num       <- choose (1, max n 1)
  let v_bounds  = (0, v_num - 1)
  let v_range   = range v_bounds                            -- :: [Int]
  subranges   <- sequenceA $ repeat (genSubrange v_bounds)  -- :: [[Int]]
  let assocns = zipWith (,) v_range subranges               -- :: [(Int, [Int])]
  return $ array v_bounds assocns

genGraph :: Gen Graph 
genGraph = sized n_genGraph

{-
instance Arbitrary Graph where 
  arbitrary :: Gen Graph 
  arbitrary = genGraph

-- There's already an Arbitrary instance for the Array 
-- type which Graph is based upon (as a type synonym)!

-- Overlapping instances for Arbitrary Graph
--         arising from a use of ‘Test.QuickCheck.Arbitrary.$dmshrink’
--       Matching instances:
--         instance [safe] (Arbitrary e, Num i,
--                          Data.Array.Base.IArray Array e, Ix i) =>
--                         Arbitrary (Array i e)
--           -- Defined in ‘HCodecs-0.5.1:Codec.Internal.Arbitrary’
-}

instance Arbitrary (Array Vertex [Vertex]) where 
  arbitrary :: Gen (Array Vertex [Vertex]) 
  arbitrary = genGraph

main :: IO ()
main = do 
  quickCheck $ prop_TerminatingRandomWalk_WellFormed    myGraph
  quickCheck $ prop_NonterminatingRandomWalk_WellFormed myGraph2
  quickCheck $ vertInGraph_TerminatingRandomWalk_WellFormed     myGraph
  quickCheck $ vertInGraph_NonterminatingRandomWalk_WellFormed  myGraph2
  -- on arbitrary graphs; on arbitrary Vertex values
  -- (A random graph may have no terminating random walks, in general.
  --  So it's safest to use 'prop_Nonterminating[...]')
  quickCheck $ prop_NonterminatingRandomWalk_WellFormed
  -- on arbitrary graphs; on Vertex values present in the graph
  quickCheck $ vertInGraph_NonterminatingRandomWalk_WellFormed
