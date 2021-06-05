module Docker where

import RIO
import qualified Network.HTTP.Simple as HTTP
import qualified Data.Aeson as Aeson
import qualified Sockets

newtype Image = Image Text
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

data CreateContainerOptions
    = CreateContainerOptions
    { image :: Image
    }

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
    manager <- Sockets.newManager "/var/run/docker.sock"
    let image = imageToText options.image
    let body = Aeson.object 
                [ ("Image", Aeson.toJSON image),
                  ("Tty", Aeson.toJSON True),
                  ("Labels", Aeson.object [("HASCI", "")]),
                  ("Cmd", "echo hello world"),
                  ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
                ]
    let req = HTTP.defaultRequest
            & HTTP.setRequestManager manager
            & HTTP.setRequestPath "/v1.41/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    res <- HTTP.httpBS req
    traceShowIO res
            