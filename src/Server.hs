module Server where

import RIO
import Core
import qualified JobHandler
import qualified Web.Scotty as Scotty
import qualified Codec.Serialise as Serialise

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