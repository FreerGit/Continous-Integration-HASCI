module Server where

import RIO
import Core

import qualified Web.Scotty as Scotty
import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Map as Map
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai.Middleware.Cors as Cors
import qualified System.Log.Logger as Logger
import qualified RIO.Text as Text
import qualified JobHandler
import qualified Github

data Config
  = Config
    { port :: Int
    }

run :: Config -> JobHandler.Service -> IO ()
run config handler = 
  Scotty.scotty config.port do
    Scotty.middleware Cors.simpleCors

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
        info <- Github.parsePushEvent body
        pipeline <- Github.fetchRemotePipeline info
        let step = Github.createCloneStep info
        number <-
          handler.queueJob info $ 
            pipeline
              { steps = NonEmpty.cons step pipeline.steps
              }
        Logger.infoM "HASCI.server" $ "Queued job" <> Core.displayBuildNumber number
        pure number

      Scotty.json $
        Aeson.object
          [ ("number", Aeson.toJSON $ Core.buildNumberToInt number)
          , ("status", "job queued")
          ]

    Scotty.get "/build/:number" do
      number <- fmap BuildNumber (Scotty.param "number")
      job <- Scotty.liftAndCatchIO
        (handler.findJob number) >>= \case
          Nothing -> Scotty.raiseStatus HTTP.Types.status404 
            "Build not found"
          Just jobFound -> pure jobFound
      Scotty.json $ jobToJSON number job

    Scotty.get "/build/:number/step/:step/logs" do
      number <- fmap BuildNumber (Scotty.param "number")
      step <- fmap StepName (Scotty.param "step")
      log <- Scotty.liftAndCatchIO $ handler.fetchLogs number step
      Scotty.raw $ fromStrictBytes $ fromMaybe "" log
    
    Scotty.get "/build" do
      jobs <- Scotty.liftAndCatchIO do
        handler.latestJobs
      Scotty.json $ fmap (\(number,job) -> jobToJSON number job) jobs


jobToJSON :: BuildNumber -> JobHandler.Job -> Aeson.Value
jobToJSON number job = 
  Aeson.object
    [ ("number", Aeson.toJSON $ Core.buildNumberToInt number)
    , ("state", Aeson.toJSON $ jobStateToText job.state)
    , ("info", Aeson.toJSON $ job.info)
    , ("steps", Aeson.toJSON steps)
    ]
  where
    steps = fmap encodeName job.pipeline.steps 
    build = case job.state of
      JobHandler.JobQueued -> Nothing
      JobHandler.JobAssigned -> Nothing
      JobHandler.JobScheduled b -> Just b
    encodeName = \step ->
      Aeson.object
        [ ("name", Aeson.String $ Core.stepNameToText step.name)
        , ("state", Aeson.String $ case build of
            Just b -> stepStateToText b step
            Nothing -> "ready"
          )
        ]

jobStateToText :: JobHandler.JobState -> Text
jobStateToText = \case
  JobHandler.JobQueued -> "queued"
  JobHandler.JobAssigned -> "assigned"
  JobHandler.JobScheduled j -> case j.state of
    BuildReady -> "ready"
    BuildRunning _ -> "running"
    BuildFinished result -> case result of
      BuildSucceeded -> "build succeeded"
      BuildFailed -> "build failed"
      BuildUnexpectedState _ -> "unexpected state"
  
stepStateToText :: Build -> Step -> Text
stepStateToText build step =
  case build.state of
    BuildRunning s -> 
      if s.step == step.name
        then "running"
        else stepNotRunning
    _ -> stepNotRunning
  where
    stepNotRunning = case Map.lookup step.name build.completedSteps of
      Just StepSucceeded -> "succeeded"
      Just (StepFailed _) -> "failed"
      Nothing -> case build.state of
        BuildFinished _ -> "skipped"
        _ -> "ready"