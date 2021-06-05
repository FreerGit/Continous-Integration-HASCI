module Core where

import RIO

data Pipeline = Pipeline
  { steps :: NonEmpty Step
  }
  deriving (Eq, Show)

data Step = Step
  { name :: StepName,
    commands :: NonEmpty Text,
    image :: Image
  }
  deriving (Eq, Show)

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult
  }
  deriving (Eq, Show)

data StepResult
  = stepFailed ContainerExitCode
  | StepSucceded
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving (Eq, Show, Ord)

newtype Image = Image Text
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

data BuildRunningState
  = BuildRunningState
  { step :: StepName
  }
  deriving (Eq, Show)

stepNameToText :: StepName -> Text
stepNameToText (Stepname step) = step

imageToText :: Image -> Text
imageToText (Image image) = image

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if exitCodeToInt exit == 0
    then StepSucceded
    else StepFailed exit

progress :: Build -> IO Build
progress build =
  case build . state of
    BuildReady -> 
      case buildHasNextStep build of
        Left result ->
          pure $ build{state = BuildFinished result}
        Right step ->
          let s = BuildRunningState { step = step.name }
          pure $ build{state = BuildRunning s}
    BuildRunning state -> do
      let exit = ContainerExitCode 0
      result = ExitCodeToStepResult exit
      pure build 
        { state = BuildReady,
        completedSteps = Map.insert state.step result build.completedSteps}
    BuildFinished _ -> pure build