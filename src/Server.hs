module Server where

import RIO
import Core

import qualified Web.Scotty as Scotty
import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import qualified RIO.NonEmpty as NonEmpty
import qualified JobHandler
import qualified Github

data Config
  = Config
    { port :: Int
    }

run :: Config -> JobHandler.Service -> IO ()
run config handler = 
  Scotty.scotty config.port do

    Scotty.post "/agent/pull" do
      cmd <- Scotty.liftAndCatchIO do
        handler.dispatchCmd
      Scotty.raw $ Serialise.serialise cmd

    Scotty.post "/agent/send" do
      msg <- fmap Serialise.deserialise Scotty.body
      Scotty.liftAndCatchIO do
        handler.processMsg msg
      Scotty.json ("message processed" :: Text)

    Scotty.post "/webhook/github" do
      body <- Scotty.body
      number <- Scotty.liftAndCatchIO do
          info <- Github.parsePushEvent (toStrictBytes body)
          traceShowIO info
          pipeline <- Github.fetchRemotePipeline info
          traceShowIO "dafdsafsd "
          traceShowIO pipeline
          traceShowIO "fdsfdsfsdfdsfdsfsd"
          let step = Github.createCloneStep info
          handler.queueJob $ pipeline
            { steps = NonEmpty.cons step pipeline.steps
            }
      Scotty.json $
        Aeson.object
          [ ("number", Aeson.toJSON $ Core.buildNumberToInt number)
          , ("status", "job queued")
          ]
      