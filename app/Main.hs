{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

-- HTML templating
import Text.Mustache

-- Postgres db
import Database.PostgreSQL.Simple as PSQL
import Data.Pool

import Data.Aeson hiding (json)
import Control.Monad.Trans
import Data.Monoid ((<>))
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

-- Some API data definitions
data Employee = Employee
  { empid   :: T.Text -- uuid
  , ssn     :: T.Text -- Must be 9 characters
  , fname   :: T.Text
  , lname   :: T.Text
  , address :: T.Text
  , deptid  :: Integer
  , payid   :: Integer
  } deriving (Generic, Show)

instance ToJSON   Employee
instance FromJSON Employee

data Department = Department
  {  } deriving (Generic, Show)

instance ToJSON   Department
instance FromJSON Department

data Manager = Manager
  {  } deriving (Generic, Show)

instance ToJSON   Manager
instance FromJSON Manager

data Pay = Pay
  {  } deriving (Generic, Show)

instance ToJSON   Pay
instance FromJSON Pay

data Project = Project
  {  } deriving (Generic, Show)

instance ToJSON   Project
instance FromJSON Project

data Dependent = Dependent
  { empssn   :: T.Text -- must be 9 characters numeric
  , dfname   :: T.Text
  , dlname   :: T.Text
  , relation :: T.Text
  } deriving (Generic, Show)

instance ToJSON   Dependent
instance FromJSON Dependent

database = defaultConnectInfo
  { connectDatabase = "postgres"
  }

main :: IO ()
main =
  do
    -- Setup a connection to the database
    pool <- createPool (connect database) close 1 10 10
    ref <- newIORef 0
    --spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    spockCfg <- defaultSpockCfg EmptySession (PCPool pool) (DummyAppState ref)
    runSpock 8080 (spock spockCfg app)

app ::SpockM Connection MySession MyAppState ()
app =
  do
     get root $
       text ("Hey there, welcome to Giant Corp Employee Action Site \n"
            <> "(unauthorized use will be met with termination.)")
     get ("hello" <//> var) $ \name ->
       do (DummyAppState ref) <- getState
          visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
          text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
     get ("SuPeRsEcReT" <//> var) $ \secretCode ->
       text ("Your secret is " <> T.pack(secretCode))
