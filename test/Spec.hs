module Main where

import Core
import Runner
import RIO

import qualified RIO.ByteString as ByteString
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified System.Process.Typed as Process
import qualified Data.Yaml as Yaml
import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Simple as HTTP
import qualified RIO.HashMap as HashMap
import qualified Docker
import qualified Agent
import qualified Server
import qualified JobHandler
import qualified JobHandler.Memory
import Test.Hspec

main :: IO ()
main = hspec do 
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker
  parallel $ do
    afterAll_ cleanupDocker $ describe "HASCI" do
      it "should run a build (success)" do
        testRunSuccess runner
      it "should run a build (failure)" do
        testRunFailure runner
      it "should share workspace between steps" do
        testSharedWorkspace docker runner
      it "should collect logs" do
        testLogCollection runner
      it "should pull images" do 
        testImagePull runner
      it "should decode pipelines from yml" do
        testYamlDecoding runner
      it "should run server and agent concurrently" do
        testServerAndAgent runner
      it "should process github webhook" do
        testWebhookTrigger runner



makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name
    , image = Docker.Image { name = image, tag = "latest"}
    , commands = NonEmpty.Partial.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline{steps = NonEmpty.Partial.fromList steps}

testPipeline :: Pipeline
testPipeline =
  makePipeline
    [ makeStep "first step" "ubuntu" ["date"]
    , makeStep "second step" "ubuntu" ["uname -r"]
    ]

emptyHooks :: Runner.Hooks
emptyHooks =
  Runner.Hooks
    { logCollected = \_ -> pure ()
    , buildUpdated = \_ -> pure ()
    }

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <- runner.prepareBuild testPipeline
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <- runner.prepareBuild $ makePipeline
              [ makeStep "Should fail" "ubuntu" ["exit 1"]
              ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps
    `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]


testSharedWorkspace :: Docker.Service -> Runner.Service -> IO ()
testSharedWorkspace docker runner = do
  build <- runner.prepareBuild $ makePipeline
          [ makeStep "Create File" "ubuntu" ["echo hello > test.txt"]
          , makeStep "Read File" "ubuntu" ["cat test.txt"]
          ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do
  expected <- newMVar $ Set.fromList ["hello", "world", "Linux"]
  let onLog :: Log -> IO ()
      onLog log = do
        remaining <- readMVar expected
        forM_ remaining $ \word -> do
          case ByteString.breakSubstring word log.output of
            (_, "") -> pure ()
            _ -> modifyMVar_ expected (pure . Set.delete word)
  let hooks = Runner.Hooks { logCollected = onLog
                           , buildUpdated = \_ -> pure () 
                           }

  build <- runner.prepareBuild $ makePipeline 
              [ makeStep "Many steps" "ubuntu" ["echo hello", "sleep 2", "echo world"]
              , makeStep "Echo Linux" "ubuntu" ["uname -s"]
              ]
  result <- runner.runBuild hooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]
  readMVar expected >>= \logs -> logs `shouldBe` Set.empty

testImagePull :: Runner.Service -> IO ()
testImagePull runner = do
  Process.readProcessStdout "docker rmi -f busybox"

  build <- runner.prepareBuild $ makePipeline
            [ makeStep "First step" "busybox" ["date"]
            ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded]

testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
  pipeline <- Yaml.decodeFileThrow "test/pipeline.sample.yml"
  build <- runner.prepareBuild pipeline
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded

testServerAndAgent :: Runner.Service -> IO ()
testServerAndAgent = do
  runServerAndAgent 9000 $ \handler -> do
    let pipeline = makePipeline
          [ makeStep "agent-test" "busybox" ["echo hello", "echo from agent"]
          , makeStep "agent-test" "busybox" ["echo hello", "echo from agent"]
          ]
    let info = JobHandler.CommitInfo
          { sha = "00000"
          , branch = "master"
          , message = "test commit"
          , author = "freergit"
          , repo = "freergit/hasci-pipeline-test"}
    number <- handler.queueJob info pipeline
    checkBuild handler number

testWebhookTrigger :: Runner.Service -> IO ()
testWebhookTrigger = 
  runServerAndAgent 9001 $ \handler -> do
    base <- HTTP.parseRequest "http://localhost:9001"
    let req =
          base
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestPath "/webhook/github"
            & HTTP.setRequestBodyFile "test/github-payload.sample.json"
    res <- HTTP.httpBS req
    let Right (Aeson.Object build) =
          Aeson.eitherDecodeStrict $ HTTP.getResponseBody res
    let Just (Aeson.Number number) = HashMap.lookup "number" build
    checkBuild handler $ Core.BuildNumber (round number)

runServerAndAgent :: Int -> (JobHandler.Service -> IO ()) -> Runner.Service -> IO ()
runServerAndAgent port callback runner = do
  handler <- JobHandler.Memory.createService
  serverThread <- Async.async do
    Server.run (Server.Config port) handler
  Async.link serverThread

  -- Just to skip annoying logs, wait for server to start 200ms
  threadDelay (1 * 1000 * 200) 

  agentThread <- Async.async do
    Agent.run (Agent.Config ("http://localhost:" <> show port) "agent1") runner
  Async.link agentThread

  callback handler
  Async.cancel serverThread
  Async.cancel agentThread
  

checkBuild :: JobHandler.Service -> BuildNumber -> IO ()
checkBuild handler number = loop
  where 
    loop = do
      Just job <- handler.findJob number
      case job.state of
        JobHandler.JobScheduled build -> do
          case build.state of
            
            BuildFinished s -> s `shouldBe` BuildSucceeded
            _ -> loop
        _ -> loop

cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=HASCI\")"
  Process.readProcessStdout 
    "docker volume rm -f $(docker volume ls -q --filter \"label=HASCI\")"
