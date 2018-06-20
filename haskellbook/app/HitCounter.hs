{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config
  { counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)

type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoop :: Text
         -> M.Map Text Integer
         -> (M.Map Text Integer, Integer)
bumpBoop k m =
  let
    newMap = M.insertWith (+) k 1 m
    newCount = 1 `fromMaybe` M.lookup k newMap
  in (newMap, newCount)

app :: Scotty ()
app =
  get "/:key" $ do
    config <- lift ask
    unprefixed <- param "key" :: Handler Text
    let
      key' = mappend (prefix config) unprefixed
      readConfig :: IO (M.Map Text Integer)
      readConfig = readIORef (counts config)
      wrappedReading :: ActionT Text (ReaderT Config IO) (M.Map Text Integer)
      wrappedReading = lift (ReaderT $ const readConfig)
    counts' <- wrappedReading
    let (newCounts, newCount) = bumpBoop key' counts'
    -- (lift . lift) (writeIORef (counts config) newCounts)
    lift (ReaderT $ const (writeIORef (counts config) newCounts))
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newCount
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config { counts = counter,  prefix = (TL.pack prefixArg) }
      runR = (flip runReaderT) config
  scottyT 3000 runR app
