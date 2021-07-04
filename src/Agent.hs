module Agent where

import RIO
import Core
import qualified Codec.Serialise as Serialise
import qualified Network.HTTP.Simple as HTTP
import qualified System.Log.Logger as Logger
import qualified Runner

newtype Name = Name String
  deriving (Eq, Show, Generic, Serialise.Serialise)

instance IsString Name where
  fromString string = Name string 

data Cmd 
  = StartBuild BuildNumber Pipeline
    deriving (Eq, Show, Generic, Serialise.Serialise)

data Msg
  = LogCollected Name BuildNumber Log
  | BuildUpdated Name BuildNumber Build
    deriving (Eq, Show, Generic, Serialise.Serialise)
  
data Config
  = Config
    { endpoint :: String
    , name :: Name
    }

run :: Config -> Runner.Service -> IO ()
run config runner = forever do
  endpoint <- HTTP.parseRequest config.endpoint
  let req = endpoint
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/agent/pull"
  do
    res <- HTTP.httpLBS req
    let cmd = Serialise.deserialise (HTTP.getResponseBody res) :: Maybe Cmd
    traverse_ (runCommand config runner) cmd
    `catch` \e -> do
      Logger.warningM "hasci.agent" "Server offline, waiting...."
      Logger.warningM "hasci.agent" $ show (e :: HTTP.HttpException)
  threadDelay (1 * 1000 * 1000)


runCommand :: Config -> Runner.Service -> Cmd -> IO ()
runCommand config runner = \case
  StartBuild number pipeline -> do
    let hooks = Runner.Hooks
          { logCollected = \log -> do
              sendMessage config $ LogCollected config.name number log
          , buildUpdated = \build -> do
              sendMessage config $ BuildUpdated config.name number build
          }
    build <- runner.prepareBuild pipeline
    void $ runner.runBuild hooks build

sendMessage :: Config -> Msg -> IO ()
sendMessage config msg = do
  base <- HTTP.parseRequest config.endpoint
  let body = Serialise.serialise msg
  let req = base
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/agent/send"
          & HTTP.setRequestBodyLBS body
  void $ HTTP.httpBS req