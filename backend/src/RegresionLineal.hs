{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module RegresionLineal where
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
  import Servant
  import System.Directory
  import Text.Blaze
  import Text.Blaze.Html.Renderer.Utf8
  import Servant.Types.SourceT (source)
  import qualified Data.Aeson.Parser
  import qualified Text.Blaze.Html

  newtype ArrayData = ArrayData [Double]
  newtype DoubleData = DoubleData Double
  newtype IntData = IntData Int

  data ArrayPost = ArrayPost {
    x :: [Double],
    y :: [Double]
  } deriving Generic

  instance FromJSON ArrayPost
  instance ToJSON ArrayPost

  data LinearRegression = LinearRegression {
      initialDataX :: [Double],
      initialDatay :: [Double],
      dataX :: Double,
      dataY :: Double,
      mediumX :: Double,
      mediumY :: Double,
      valuesMinusX :: [Double],
      valuesMinusY :: [Double],
      multiplyXY :: [Double],
      powValuesX :: [Double],
      powValuesY :: [Double],
      totalPowValX :: Double,
      totalPowValY :: Double,
      totalXY :: Double,
      sxy :: Double,
      sx :: Double,
      sy :: Double,
      r :: Double,
      relation :: Double
  } deriving(Eq, Show, Generic)

  instance ToJSON LinearRegression

  sumArray :: ArrayData -> Double
  sumArray (ArrayData array) = foldl (\ acc curr -> acc + curr) 0 array

  getMedium :: DoubleData -> IntData -> Double
  getMedium (DoubleData val) (IntData len) =  val /  (fromIntegral len)

  getValueMinusMedium :: DoubleData -> ArrayData -> [Double]
  getValueMinusMedium (DoubleData medium) (ArrayData array) = map (\val -> val - medium) array

  getPowValueMinusMedium :: ArrayData -> [Double]
  getPowValueMinusMedium (ArrayData array) = map (\val -> (val ^ 2)) array

  getMultiplyArray :: ArrayData -> ArrayData -> [Double]
  getMultiplyArray (ArrayData []) (ArrayData []) = []
  getMultiplyArray (ArrayData (xx:xxy)) (ArrayData (xy:xyy)) = xx * xy : getMultiplyArray (ArrayData xxy) (ArrayData xyy)

  getSquare :: DoubleData -> Double
  getSquare (DoubleData value) = sqrt value

  getLinealRelation :: DoubleData -> DoubleData -> DoubleData -> Double
  getLinealRelation (DoubleData sxy) (DoubleData sx) (DoubleData sy) = sxy / (sx * sy)

  getDeterminationCoeficient :: DoubleData -> Double
  getDeterminationCoeficient (DoubleData r) = (r ^ 2) * 100

  getLinealRegression :: ArrayData -> ArrayData -> LinearRegression
  getLinealRegression (ArrayData x) (ArrayData y) = 
    let dX = sumArray (ArrayData x)
        dY = sumArray (ArrayData y)
        mdX = getMedium (DoubleData dX) (IntData (length  x))
        mdY = getMedium (DoubleData dY) (IntData (length y))
        vMsX = getValueMinusMedium (DoubleData mdX) (ArrayData x) 
        vMsY = getValueMinusMedium (DoubleData mdY) (ArrayData y)
        pValX = getPowValueMinusMedium (ArrayData vMsX)
        pValY = getPowValueMinusMedium (ArrayData vMsY)
        multiXY = getMultiplyArray (ArrayData vMsX) (ArrayData vMsY)
        totPowValX = sumArray (ArrayData pValX)
        totPowValY = sumArray (ArrayData pValY)
        totXY = sumArray (ArrayData multiXY)
        totSxy = getMedium (DoubleData totXY) (IntData (length x))
        totSx = getSquare (DoubleData (getMedium (DoubleData totPowValX) (IntData (length x))))
        totSy = getSquare (DoubleData (getMedium (DoubleData totPowValY) (IntData (length y))))
        totR = getLinealRelation (DoubleData totSxy) (DoubleData totSx) (DoubleData totSy)
        relationLinear = getDeterminationCoeficient (DoubleData totR)

    in LinearRegression {
          initialDataX = x,
          initialDatay = y,
          dataX = dX,
          dataY = dY,
          mediumX = mdX,
          mediumY = mdY,
          valuesMinusX = vMsX,
          valuesMinusY = vMsY,
          powValuesX = pValX,
          powValuesY = pValY,
          multiplyXY = multiXY,
          totalPowValX = totPowValX,
          totalPowValY = totPowValY,
          totalXY = totXY,
          sxy = totSxy,
          sx = totSx,
          sy = totSy,
          r = totR,
          relation = relationLinear
        }
