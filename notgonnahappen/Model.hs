{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity Session) where
  toJSON (Entity sid s) = object
    [ "id" .= (String $ toPathPiece sid)
    , "start" .=  sessionStart s
    , "end" .= sessionEnd s
    , "tempo" .= sessionTempo s
    , "songId" .= sessionSongId s
    ]

instance FromJSON Session where
  parseJSON (Object o) = Session
    <$> o .: "start"
    <*> o .: "end"
    <*> o .: "tempo"
    <*> o .: "songId"
  parseJSON _ = mzero
