{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Prelude ()
import Prelude.Compat
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import RegresionLineal

type UserAPI1 = "regresionL" :> ReqBody '[JSON] ArrayPost :> Post '[JSON] Regresion

data Regresion = Regresion
  {
    body :: LinearRegression
  } deriving Generic

instance ToJSON Regresion

linearRegresionPost :: ArrayPost -> Regresion
linearRegresionPost dataPost = Regresion body'
    where body' = getLinealRegression (ArrayData (x dataPost)) (ArrayData (y dataPost))

corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"] }

server1 :: Server UserAPI1
server1 = regresionL where
  regresionL :: ArrayPost -> Handler Regresion
  regresionL dataPost = return (linearRegresionPost dataPost)

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

startApp :: IO ()
startApp = run 8080 $ corsWithContentType $ app1 

