{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Yesod
import Data.Text
import Data.Aeson.Types
import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name Text
  email Text
  photoUrl Text
  deriving Show
|]

data App = App { conn :: ConnectionPool }

mkYesod "App" [parseRoutes|
/ HomeR GET POST
/#PersonId PersonR GET PUT DELETE
|]

instance Yesod App where
  makeSessionBackend _ =
    fmap Just $ defaultClientSessionBackend (24 * 60 * 7) "client_session_key.aes"

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    master <- getYesod
    let pool = conn master
    runSqlPool f pool

instance ToJSON Person where
  toJSON Person {..} = 
    object
      ["name" .= personName
      , "email" .= personEmail
      , "photo" .= personPhotoUrl
      ]

instance ToJSON (Entity Person) where
  toJSON (Entity key (Person {..})) = 
    object
      ["id" .= key
      , "name" .= personName
      , "email" .= personEmail
      , "photo" .= personPhotoUrl
      ]

instance FromJSON Person where
  parseJSON = 
    withObject "Person" $ \v ->
      Person <$> v .: "name" <*> v .: "email" <*> v .: "photo"

getHomeR :: Handler Value
getHomeR = do
  personList <- runDB $ selectList [] [Desc PersonId]
  return $ object ["data" .= personList]

postHomeR :: Handler Value
postHomeR = do
  person <- requireCheckJsonBody :: Handler Person
  runDB $ insert person
  return $ object ["data" .= person]

getPersonR :: PersonId -> Handler Value
getPersonR personId = do
  person <- runDB $ get404 personId
  return $ object ["data" .= person]

putPersonR :: PersonId -> Handler Value
putPersonR personId = do
  person <- runDB $ get404 personId
  updatedPerson <- requireCheckJsonBody :: Handler Person
  runDB $ update personId [PersonName =. personName updatedPerson, PersonEmail =. personEmail updatedPerson,PersonPhotoUrl =. personPhotoUrl updatedPerson]
  return $ object ["data" .= updatedPerson]

deletePersonR :: PersonId -> Handler Value
deletePersonR personId = do
  person <- runDB $ get404 personId
  runDB $ delete personId
  return $ object ["data" .= pack "Person deleted successfully!"]

connStr :: ConnectionString
connStr = "dbname=bd_projeto host=localhost user=postgres password=root port=5432"

main :: IO ()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
  warp 3000 (App pool)