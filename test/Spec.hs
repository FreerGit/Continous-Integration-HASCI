module Main where

import Core
import RIO
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified Docker

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

main :: IO ()
main = pure ()