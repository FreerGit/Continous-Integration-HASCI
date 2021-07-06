module Cli where

import RIO

import qualified Agent
import qualified Server
import qualified Docker
import qualified Runner
import qualified JobHandler.Memory

import qualified System.Log.Logger as Logger
import qualified UI.Butcher.Monadic as Butcher

data Command 
  = StartServer Server.Config
  | StartAgent Agent.Config

main :: IO ()
main = Butcher.mainFromCmdParserWithHelpDesc $ \helpDesc -> do
  Butcher.addCmdSynopsis "HASCI CI command line utiltity"
  Butcher.addHelpCommand2 helpDesc

  Butcher.addCmd "start-server" do
    Butcher.addCmdSynopsis "start server node"
    port <- Butcher.addParamString "PORT" $
      Butcher.paramHelpStr "Server port"
        <> Butcher.paramDefault (show defaultPort)

    Butcher.addCmdImpl $
      case readMaybe port of
        Nothing -> throwString "Port must be a number"
        Just p -> do
          let config = Server.Config { port = p }
          runCommand $ StartServer config
  
  Butcher.addCmd "start-agent" do
    Butcher.addCmdSynopsis "start agent node"
    endpoint <- Butcher.addParamString "ENDPOINT" $
      Butcher.paramHelpStr "Server Endpoint"
        <> Butcher.paramSuggestions [defaultEndpoint]
        <> Butcher.paramDefault defaultEndpoint
    name <- Butcher.addParamString "NAME" $
      Butcher.paramHelpStr "Agent node name"
        <> Butcher.paramDefault defaultName

    Butcher.addCmdImpl do
      let config = Agent.Config { endpoint = endpoint
                                , name = Agent.Name name } 
      runCommand $ StartAgent config
  
  Butcher.addCmd "cleanup-docker" do
    Butcher.addCmdSynopsis "cleanup docker container/images"
    Butcher.addCmdImpl do
      Docker.cleanupDocker


runCommand :: Command -> IO ()
runCommand = \case
  StartServer config -> do
    Logger.infoM "HASCI Server" "Server Starting...."
    handler <- JobHandler.Memory.createService
    Server.run config handler
  StartAgent config -> do
    Logger.infoM "HASCI Agent" "Agent Starting...."
    docker <- Docker.createService
    runner <- Runner.createService docker
    Agent.run config runner

defaultPort :: Int
defaultPort = 9000

defaultEndpoint :: String
defaultEndpoint = "http://localhost:" <> show defaultPort

defaultName :: String
defaultName = "DEFAULT"