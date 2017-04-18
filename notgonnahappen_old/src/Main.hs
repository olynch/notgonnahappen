module Main where

import Protolude
import Yesod
import Database.Persist.Postgresql
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad (mzero)
import Data.Time.Clock
import Data.Aeson

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  deriving Show Eq
Song
  title Text
  user UserId
  deriving Show Eq
Session
  tempo Int
  song SongId
  start UTCTime
  end UTCTime
  deriving Show Eq
|]

instance ToJSON (Entity User) where
  toJSON (Entity pid p) = object
    [ "id" .= (String $ toPathPiece pid)
    , "name" .= userName p
    ]

instance FromJSON User where
  parseJSON (Object o) = User
    <$> o .: "name"
  parseJSON _ = mzero

instance ToJSON (Entity Song) where
  toJSON (Entity pid s) = object
    [ "id" .= (String $ toPathPiece pid)
    , "user_id" .= (String $ toPathPiece $ songUser s)
    , "title" .= songTitle s
    ]

instance FromJSON Song where
  parseJSON (Object o) = Song
    <$> o .: "title"
    <*> o .: "user_id"
  parseJSON _ = mzero

instance ToJSON (Entity Session) where
  toJSON (Entity pid s) = object
    [ "id" .= (String $ toPathPiece pid)
    , "tempo" .= sessionTempo s
    , "song" .= (String $ toPathPiece $ sessionSong s)
    , "start" .= sessionStart s
    , "end" .= sessionEnd s
    ]

instance FromJSON Session where
  parseJSON (Object o) = Session
    <$> o .: "tempo"
    <*> o .: "song"
    <*> o .: "start"
    <*> o .: "end"
  parseJSON _ = mzero

data App = App ConnectionPool

mkYesod "App" [parseRoutes|
/ HomeR GET
/metronome MetronomeR GET POST
/log/#Text LogR GET
|]

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    App pool <- getYesod
    runSqlPool action pool

getHomeR :: Handler Html
getHomeR =
  defaultLayout
    [whamlet|
      <h1> A logging metronome - if you don't practice every day it's not gonna happen!
    |]

getMetronomeR :: Handler Html
getMetronomeR = defaultLayout
    [whamlet| Metronome here |]

postMetronomeR :: Handler Value
postMetronomeR = do
  return $ object []

getLogR :: Text -> Handler Html
getLogR name = defaultLayout
    [whamlet| Log for #{name} |]

openConnectionCount :: Int
openConnectionCount = 10

connStr :: ConnectionString
connStr = "host=localhost user=olynch password=test dbname=notgonnahappen port=5432"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr openConnectionCount $ \pool ->
  liftIO $ do
    flip runSqlPersistMPool pool $ runMigration migrateAll
    warp 3000 $ App pool
