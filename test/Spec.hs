module Main where

import Core
import RIO
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified Docker
import qualified RIO.Map as Map
import qualified System.Process.Typed as Process
import Test.Hspec


makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name
    , image = Docker.Image image
    , commands = NonEmpty.Partial.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline{steps = NonEmpty.Partial.fromList steps}

testPipeline :: Pipeline
testPipeline =
  makePipeline
    [ makeStep "first step" "ubuntu" ["date"]
    , makeStep "second step" "ubuntu" ["umame -r"]
    ]

testBuild :: Build
testBuild =
  Build
    { pipeline = testPipeline
    , state = BuildReady
    , completedSteps = mempty
    }

testRunSuccess :: Docker.Service -> IO ()
testRunSuccess docker = do
  result <- runBuild docker testBuild
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

runBuild :: Docker.Service -> Build -> IO Build
runBuild docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished _ ->
      pure newBuild
    _ -> do
      threadDelay (1* 1000 * 1000)
      runBuild docker newBuild

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  beforeAll cleanupDocker $ describe "HASCI" do
    it "should run a build (success)" do
      testRunSuccess docker

cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=HASCI\")"