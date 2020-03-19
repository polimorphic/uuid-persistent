{-# LANGUAGE OverloadedStrings #-}

module Data.UUID.Persist.Instances () where

import Data.Text (pack)
import Data.UUID.PathPieces.Instances ()
import Data.UUID.Types (UUID, fromASCIIBytes, toASCIIBytes)
import Database.Persist
    ( PersistField, PersistValue(PersistDbSpecific), SqlType(SqlOther)
    , fromPersistValue, toPersistValue
    )
import Database.Persist.Sql (PersistFieldSql, sqlType)

instance PersistField UUID where
    toPersistValue = PersistDbSpecific . toASCIIBytes
    fromPersistValue (PersistDbSpecific b) = case fromASCIIBytes b of
        Nothing -> Left $ "Expected UUID; received: " <> pack (show b)
        Just u -> Right u
    fromPersistValue x = Left $ "Expected PersistDbSpecific; received: " <> pack (show x)

instance PersistFieldSql UUID where
    sqlType _ = SqlOther "uuid"
