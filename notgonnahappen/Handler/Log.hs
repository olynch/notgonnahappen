module Handler.Log where

import Import
import Calendar

getLogR :: Handler Html
getLogR = do
  (_, user) <- requireAuthPair
  defaultLayout $ do
    setTitle . toHtml $ userIdent user <> "'s Practice Log"
    $(widgetFile "profile")

getCalendarR :: SongId -> Int -> Int -> Handler SVGContent
getCalendarR sid year month = do
  (userId, _) <- requireAuthPair
  Song _ songUserId <- runDB $ get404 sid
  if songUserId == userId then do
      diag <- calendarPage sid year month
      return $ diaToSVG diag
    else
      notFound

    

      
