module Docker where

import RIO
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as Aeson.Types
import qualified Network.HTTP.Simple as HTTP
import qualified Data.Aeson as Aeson
import qualified Sockets

newtype Image = Image Text
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

newtype ContainerId = ContainerId Text
  deriving (Eq, Show)

newtype Volume = Volume Text
  deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId id) = id

volumeToText :: Volume -> Text
volumeToText (Volume v) = v

data CreateContainerOptions
    = CreateContainerOptions
    { image :: Image
    , script :: Text
    }

data Service 
  = Service
    { createContainer :: CreateContainerOptions -> IO ContainerId
    , startContainer :: ContainerId -> IO ()
    , containerStatus :: ContainerId -> IO ContainerStatus
    , createVolume :: IO Volume
    }

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Eq, Show)

type RequestBuilder = Text -> HTTP.Request

createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ makeReq options = do
    let image = imageToText options.image
    let body = Aeson.object 
            [ ("Image", Aeson.toJSON image)
            ,  ("Tty", Aeson.toJSON True)
            ,  ("Labels", Aeson.object [("HASCI", "")])
            ,  ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
            ,  ("Cmd", "echo \"$HASCI_SCRIPT\" | /bin/sh")
            ,  ("Env", Aeson.toJSON ["HASCI_SCRIPT=" <> options.script])
            ]

    let req = makeReq "/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body

    let parser = Aeson.withObject "create-container" $ \obj -> do
          cId <- obj .: "Id"
          pure $ ContainerId cId
    
    res <- HTTP.httpBS req
    parseResponse res parser

parseResponse 
  :: HTTP.Response ByteString 
  -> (Aeson.Value -> Aeson.Types.Parser a)
  -> IO a
parseResponse res parser = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value

  case result of
    Left e -> throwString e
    Right status -> pure status

startContainer_ :: RequestBuilder -> ContainerId -> IO ()
startContainer_ makeReq container = do
  let path = "/containers/" <> containerIdToText container <> "/start"
  
  let req = makeReq path
        & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS req

containerStatus_ :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus_ makeReq container = do
  let parser = Aeson.withObject "container-inspect" $ \obj -> do
        state <- obj .: "State"
        status <- state .: "Status"
        case status of
          "running" -> pure ContainerRunning
          "exited" -> do
            code <- state .: "ExitCode"
            pure $ ContainerExited (ContainerExitCode code)
          other -> pure $ ContainerOther other

  let req = makeReq $ "/containers/" <> containerIdToText container <> "/json"

  res <- HTTP.httpBS req
  parseResponse res parser

createVolume_ :: RequestBuilder -> IO Volume
createVolume_ makeReq = do
  let body = Aeson.object
              [ ("Labels", Aeson.object [("HASCI", "")])
              ]
  let req = makeReq "/volumes/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  let parser = Aeson.withObject "create-volume" $ \obj -> do
        name <- obj .: "Name"
        pure $ Volume name

  res <- HTTP.httpBS req
  parseResponse res parser

createService :: IO Service
createService = do
  manager <- Sockets.newManager "/var/run/docker.sock"
  let makeReq :: RequestBuilder
      makeReq path = 
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath (encodeUtf8 $ "/v1.41" <> path)
  pure Service
    { createContainer = createContainer_ makeReq
    , startContainer = startContainer_ makeReq
    , containerStatus = containerStatus_ makeReq 
    , createVolume = createVolume_ makeReq
    }