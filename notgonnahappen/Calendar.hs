module Calendar
  ( SVGContent(..)
  , diaToSVG
  , calendarPage
  , calendar
  , snakeGraph
  ) where

import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB.Linear
import Data.List (scanl)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg.Core as Svg
import Import

newtype SVGContent = SVGContent
  { unSVGContent :: LByteString
  }

instance ToContent SVGContent where
  toContent s = toContent $ unSVGContent s

instance ToTypedContent SVGContent where
  toTypedContent s = toTypedContent (typeSvg, toContent s)

diaToSVG :: Diagram B -> SVGContent
diaToSVG dia =
  SVGContent $ Svg.renderBS $ renderDia SVG (SVGOptions (mkWidth 250) Nothing "" [] True) dia

calendarPage :: SongId -> Int -> Int -> Handler (Diagram B)
calendarPage sid year month = do
  let startDate = fromGregorian (fromIntegral year) month 1
  let endDate =
        fromGregorian (fromIntegral year) month (gregorianMonthLength (fromIntegral year) month)
  let startTime = UTCTime startDate (secondsToDiffTime 0)
  let endTime = UTCTime endDate (secondsToDiffTime 86400)
  sessions <-
    runDB $
    selectList
      [SessionSongId ==. sid, SessionStart >=. startTime, SessionStart <=. endTime]
      [Asc SessionStart]
  return $ calendar year month (sessionsToPracticeDays year month (map entityVal sessions))

sessionsToPracticeDays :: Int -> Int -> [Session] -> [PracticeDay]
sessionsToPracticeDays year month sessions =
  let monthLen = gregorianMonthLength (fromIntegral year) month
      lengthAndTempo (Session s e t _) =
        (fromRational $ toRational $ diffUTCTime e s, fromIntegral t)
      groupByDate :: Int -> [Session] -> [PracticeDay]
      groupByDate date (sess:rest) =
        let (_, _, sessDoM) = toGregorian $ utctDay $ sessionStart sess
        in if date == monthLen + 1
             then []
             else if date == sessDoM
                    then let day:restOfMonth = groupByDate date rest
                         in (lengthAndTempo sess : day) : restOfMonth
                    else [] : groupByDate (date + 1) (sess : rest)
      groupByDate date [] =
        if date == monthLen + 1
          then []
          else replicate (monthLen + 1 - date) []
  in groupByDate 1 sessions

calendar :: Int -> Int -> [PracticeDay] -> Diagram B
calendar year month days =
  let startDate = fromGregorian (fromIntegral year) month 1
      (_, _, startDateDOW') = toWeekDate startDate
      startDateDOW = startDateDOW' `rem` 7
      monthLen = gregorianMonthLength (fromIntegral year) month
      base = calendarBase monthLen startDateDOW
      (firstWeek, restDays) = splitAt (7 - startDateDOW) days
      restWeeks = splits 7 (impureNonNull restDays)
  in base <> snakeGraph (impureNonNull firstWeek) # translate (fromIntegral startDateDOW ^& (-0.5)) <>
     atPoints
       [p2 (0, (-1) * (0.6 + fromIntegral y)) | y <- [1 .. (length restWeeks)]]
       (map snakeGraph (toNullable restWeeks))

splits :: Int -> NonNull [a] -> NonNull [NonNull [a]]
splits n xs =
  let (firstN, post) = splitAt n (toNullable xs)
  in ncons (impureNonNull firstN) (maybe [] (toNullable . splits n) (fromNullable post))

calendarCell :: Diagram B
calendarCell = fromOffsets [(-0.9) * unitY, 0.9 * unitX] # translateY (-0.1) # lwG 0.005

calendarText :: String -> Diagram B
calendarText s = text s # fontSize (local 0.15) # translate (r2 (0.15, -0.15))

calendarBase :: Int -> Int -> Diagram B
calendarBase monthLen startDateDOW =
  position
    [ (p2 (fromIntegral x, fromIntegral y), calendarCell <> calendarText (show (i + 1)))
    | i <- [0 .. (monthLen - 1)]
    , let x = (i + startDateDOW) `rem` 7
    , let y = (-1) * (i + startDateDOW) `quot` 7
    ]

tempoToHue :: Double -> Double
tempoToHue tempo =
  hue (toSRGB blue) +
  (hue (toSRGB red) - hue (toSRGB blue)) * (log tempo - log minTempo) /
  (log maxTempo - log minTempo)

rgbToColor
  :: Fractional a
  => RGB a -> Colour a
rgbToColor r = rgb (channelRed r) (channelGreen r) (channelBlue r)

tempoToColor :: Double -> Colour Double
tempoToColor tempo = rgbToColor (hsv (tempoToHue tempo) 1 1)

practiceDayToStops :: PracticeDay -> [(Colour Double, Double, Double)]
practiceDayToStops day' =
  concat $ do
    let len = sum $ map (^. _1) day'
    let day = map (over _1 (* (1 / len))) day'
    (f, r) <- (,) <$> headMay day <*> tailMay day
    let sessions =
          scanl
            (\(_, _, p, _) (l2, t) -> (tempoToColor t, p, p + l2, l2))
            (tempoToColor (f ^. _2), 0, f ^. _1, f ^. _1)
            r
    let stops = sessions >>= (\(c, p1, _, l) -> [(c, p1 + l / 3, 1), (c, p1 + 2 * l / 3, 1)])
    firstStop <- headMay stops
    lastStop <- lastMay stops
    return $ [set _2 0 firstStop] ++ stops ++ [set _2 1 lastStop]

-- the number of seconds in an hour
hours :: Double
hours = 3600

snakeGraph :: NonNull [PracticeDay] -> Diagram B
snakeGraph days =
  let practiceTimes = impureNonNull $ map ((/ (8 * hours)) . sum . map (^. _1)) (toNullable days)
      len = length practiceTimes
      top =
        [p2 (0, head practiceTimes)] ++
        concat
          (zipWith
             (\x y -> [p2 (x + 1 / 6, y), p2 (x + 1 / 2, y), p2 (x + 5 / 6, y)])
             [0 ..]
             (toNullable practiceTimes)) ++
        [p2 (fromIntegral len, last practiceTimes)]
      bottom = reverse $ map reflectY top
      snake =
        closeLine $
        bspline top <> fromOffsets [(-2 * last practiceTimes) *^ unitY] <> bspline bottom
      stops =
        mkStops $
        concat $
        zipWith
          (\i day -> map (over _2 $ (/ fromIntegral len) . (+ i)) $ practiceDayToStops day)
          [0 ..]
          (toNullable days)
      gradient = mkLinearGradient stops (0 ^& 0) (fromIntegral len ^& 0) GradPad
  in strokeLoop snake # fillTexture gradient # lw 0 # translateY (head practiceTimes)
