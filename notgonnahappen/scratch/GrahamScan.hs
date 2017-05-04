import Diagrams.Prelude
import Diagrams.TwoD.Vector
import Diagrams.Backend.Cairo.CmdLine
import Codec.Picture.Gif
import Data.List
import GHC.Exts
import System.Random



grahamScan :: [P2 Double] -> [Diagram B]
grahamScan ps = let
    p0 = minimumBy 
        (\p1 p2 ->
            compare (p1^._y) (p2^._y) `mappend` compare (p1^._x) (p2^._x))
        ps
    angleWithP0 p = (p .-. p0) ^. _theta
    sorted = p0:(sortWith angleWithP0 $ tail ps) ++ [p0]
    circles = repeat $ circle 0.2 # fc black
    grahamStep _ [] = []
    grahamStep (s0:s1:s2:ss) (r0:rs) = let
        (newS0:newS1:newSS, newR) = 
                if not $ leftTurn (s0 .-. s1) (s1 .-. s2)
                then (r0:s0:s1:s2:ss, rs)
                else (s0:s2:ss, r0:rs)
             in ((atPoints ps circles <> (fromVertices [newS0, newS1] # lc blue) <> (fromVertices (newS1:newSS) # lc red)) # bg white):(grahamStep (newS0:newS1:newSS) newR)
    grahamStep s _ = [fromVertices s]
    s0:s1:s2:r = sorted
    in grahamStep [s2,s1,s0] r

main = do
  g <- newStdGen
  let testList = map p2 $ (uncurry zip) $ splitAt 250 $ take 500 $ randomRs (0-40, 40) g
  gifMain $ zip (grahamScan testList) (repeat 15 :: [GifDelay])
-- main = mainWith $ head $ grahamScan testList2
