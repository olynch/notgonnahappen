module Handler.RandomCal where

import Import
import Seed
import Calendar
import Data.Time.Calendar

getRandomCalR :: Int -> Int -> Handler SVGContent
getRandomCalR year month = do
  seed <- liftIO $ randomSeed
  let n = gregorianMonthLength (fromIntegral year) month
  let days = generatePracticeDays n seed exampleCalParams
  let diag = calendar year month days
  return $ diaToSVG diag
