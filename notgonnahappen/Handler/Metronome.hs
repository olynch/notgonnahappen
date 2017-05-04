module Handler.Metronome where

import Import
import Text.Julius (RawJS (..))

getMetronomeR :: Handler Html
getMetronomeR = do
  mUserId <- maybeAuthId
  songs <- case mUserId of
             Just userId -> runDB $ selectList [SongUserId ==. userId] [Asc SongTitle]
             Nothing -> return []
  let loggedIn = isJust mUserId
  defaultLayout $ do
    let (metronomeCanvasId, tempoId, startStopId, songChooserId) = metronomeIds
    setTitle "Metronome -- Not Gonna Happen"
    $(widgetFile "metronome")

metronomeIds :: (Text, Text, Text, Text)
metronomeIds = ("js-metronomeCanvasId", "js-tempoId", "js-startStopId", "js-songChooserId")

postMetronomeR :: Handler Value
postMetronomeR = do
  session <- requireJsonBody :: Handler Session
  sid <- runDB $ insert session
  return $ object ["sessionId" .= sid]
