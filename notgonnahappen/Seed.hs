module Seed
  ( randomSeed
  , exampleCalParams
  , practiceDaysToSessions
  , generatePracticeDays
  ) where

import Control.Lens
import Control.Monad.ST
import Control.Monad (zipWithM)
import Data.List (scanl)
import Data.Time.Clock
import Import
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Distribution.Uniform
import System.Random.MWC

{-
  We have lots of different things we need to be random
  - Amount of time spent each day
    - Normal distribution
    - Should have enough variation to be occasionally 0
    - floored at 0
  - Mean tempo for each day
    - Should increase slowly through the month
    - Slow exponential + normal factor
  - Distribution of tempo each day
    - Two factors -- length at each tempo and tempo
    - length at each tempo should be normal
    - number of tempi should be rough proportional to length
 -}
data CalParams = CalParams
  { practiceTimeM :: Double
  , practiceTimeSD :: Double
  , startTempo :: Double
  , endTempo :: Double
  , tempoSD :: Double
  , dayTempoRange :: Double
  , dayLengthM :: Double
  , dayLengthSD :: Double
  }

exampleCalParams :: CalParams
exampleCalParams =
  CalParams
  { practiceTimeM = 1.0
  , practiceTimeSD = 0.75
  , startTempo = 40.0
  , endTempo = 200.0
  , tempoSD = 10.0
  , dayTempoRange = 40.0
  , dayLengthM = 1 / 6
  , dayLengthSD = 1 / 18
  }

randomSeed :: IO Seed
randomSeed = do
  gen <- createSystemRandom
  save gen

practiceDaysToSessions :: SongId -> Int -> Int -> Int -> [PracticeDay] -> [Session]
practiceDaysToSessions sid year month n pdays = concat $ zipWith dayToSessions pdays [1 .. n]
  where
    dayToSessions pday i =
      let day = fromGregorian (fromIntegral year) month i
          offsets = scanl (+) 1.0 (map (^. _1) pday)
          times = map (UTCTime day . secondsToDiffTime . floor . (* 3600)) offsets
      in zipWith3
           (\tempo t0 t1 -> Session t0 t1 (floor tempo) sid)
           (map (^. _2) pday)
           times
           (tail (impureNonNull times))

generatePracticeDays :: Int -> Seed -> CalParams -> [PracticeDay]
generatePracticeDays n seed params@CalParams {..} =
  runST $ do
    let practiceTimeDist = normalDistr practiceTimeM practiceTimeSD
    let tempoDist = normalDistr 0.0 tempoSD
    gen <- restore seed
    lengths <- fmap (map (max 0)) (replicateM n (genContVar practiceTimeDist gen))
    varyTempi <- replicateM n $ genContVar tempoDist gen
    let r = (log endTempo - log startTempo) / fromIntegral n
    let baseTempi = map ((* startTempo) . exp . (* r) . fromIntegral) [0 .. (n - 1)]
    let tempoMins = zipWith (+) varyTempi baseTempi
    map (map (over _1 (* 3600))) <$> zipWithM (generateDay params gen) lengths tempoMins

generateDay :: CalParams -> GenST s -> Double -> Double -> ST s PracticeDay
generateDay CalParams {..} gen len tempoM = do
  let k = floor (len / dayLengthSD)
  let tempoDist = uniformDistr tempoM (tempoM + dayTempoRange)
  let dayLengthDist = normalDistr dayLengthM dayLengthSD
  tempi <- fmap sort (replicateM k (genContVar tempoDist gen))
  lengths' <- fmap (map (max 0)) (replicateM k (genContVar dayLengthDist gen))
  let realLength = sum lengths'
  let lengths = map (* (len / realLength)) lengths'
  return $ zip lengths tempi
