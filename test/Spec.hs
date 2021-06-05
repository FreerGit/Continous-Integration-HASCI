module Main where

import Core
import RIO
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name
    , image = Image image
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

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
 where
  allSucceeded = List.all ((==) stepSucceeded) build . completedSteps
  nextStep = List.find f build . pipeline . steps
  f step = not $ Map.member step . name build . completedSteps

main :: IO ()
main = pure ()