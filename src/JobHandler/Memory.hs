module JobHandler.Memory where

import RIO
import Core

import qualified RIO.Map as Map
import qualified Control.Concurrent.STM as STM
import qualified RIO.List as List
import qualified JobHandler
import qualified Agent

data State
  = State
    { jobs :: Map BuildNumber JobHandler.Job
    , nextBuild :: Int
    , logs :: Map (BuildNumber, StepName) ByteString
    } deriving (Eq, Show)

createService :: IO JobHandler.Service
createService = do
  state <- STM.newTVarIO State
    { jobs = mempty
    , nextBuild = 1
    , logs = mempty
    }
  pure JobHandler.Service
    { queueJob = \pipeline -> STM.atomically do
          STM.stateTVar state $ queueJob_ pipeline
    , findJob = \number -> STM.atomically do
          s <- STM.readTVar state
          pure $ findJob_ number s
    , dispatchCmd = STM.atomically do
          STM.stateTVar state dispatchCmd_
    , processMsg = \msg -> STM.atomically do
          STM.modifyTVar' state $ processMsg_ msg 
    , fetchLogs = \number step -> STM.atomically do
          s <- STM.readTVar state
          pure $ fetchLogs_ number step s
    , latestJobs = STM.atomically do
          s <- STM.readTVar state
          pure $ latestJobs_ s
    }

queueJob_ :: Pipeline -> State -> (BuildNumber, State)
queueJob_ pipeline state = 
  (number, updatedState)
  where
    number = BuildNumber state.nextBuild
    job = JobHandler.Job
      { pipeline = pipeline
      , state = JobHandler.JobQueued
      }
    updatedState =
      state
        { jobs = Map.insert number job state.jobs
        , nextBuild = state.nextBuild + 1
        }

findJob_ :: BuildNumber -> State -> Maybe JobHandler.Job
findJob_ number state = do
  Map.lookup number state.jobs

dispatchCmd_ :: State -> (Maybe Agent.Cmd, State)
dispatchCmd_ state = 
  case List.find queued $ Map.toList state.jobs of
    Just (number, job) ->
      let updateJob = job{state = JobHandler.JobAssigned}
          updatedState = Map.insert number updateJob state.jobs
          cmd = Just $ Agent.StartBuild number job.pipeline
      in (cmd, state{ jobs = updatedState })
    _ -> (Nothing, state)
  where
    queued (_, job) = job.state == JobHandler.JobQueued

processMsg_ :: Agent.Msg -> State -> State
processMsg_ msg state = case msg of
  Agent.BuildUpdated name number build ->
    let f job = job{state = JobHandler.JobScheduled build}
    in state{jobs = Map.adjust f number state.jobs}
  Agent.LogCollected name number log ->
    let updatedLogs
          = Map.insertWith (flip mappend) (number, log.step) log.output state.logs
    in state{logs = updatedLogs}

fetchLogs_ :: BuildNumber -> StepName -> State -> Maybe ByteString
fetchLogs_ number step state =
  Map.lookup (number, step) state.logs 

latestJobs_ :: State -> [(BuildNumber, JobHandler.Job)]
latestJobs_ state = List.reverse $ Map.toList state.jobs