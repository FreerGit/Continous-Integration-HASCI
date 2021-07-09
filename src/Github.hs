module Github where

import RIO
import Core
import Data.Aeson ((.:))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Simple as HTTP
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified RIO.Text as Text
import qualified RIO.Lens as Lens
import qualified Data.Aeson.Lens as Lens
import qualified JobHandler
import qualified Docker

parsePushEvent :: ByteString -> IO JobHandler.CommitInfo
parsePushEvent body = pure 
  JobHandler.CommitInfo
    { sha = body ^. sha
    , repo = body ^. repoName
    , branch = dropPrefix $ body ^. branch
    , message = body ^. message
    , author = body ^. author
    }
  where
    firstEvent = Lens.nth 0
    payload = firstEvent . Lens.key "payload"
    repo = firstEvent . Lens.key "repo"
    sha = payload . Lens.key "head" . Lens._String
    repoName = repo . Lens.key "name" . Lens._String
    branch = payload . Lens.key "ref" . Lens._String
    dropPrefix = Text.dropPrefix "refs/heads/"
    commits = payload . Lens.key "commits" . Lens.nth 0
    message = commits . Lens.key "message" . Lens._String
    author = commits . Lens.key "author" 
        . Lens.key "name" . Lens._String

fetchRemotePipeline :: JobHandler.CommitInfo -> IO Pipeline
fetchRemotePipeline info = do
  endpoint <- HTTP.parseRequest "https://api.github.com"
  let path = "/repos/" <> info.repo <> "/contents/.hasci.yml"
  let req = endpoint
          & HTTP.setRequestPath (encodeUtf8 path)
          & HTTP.setRequestSecure True
          & HTTP.addToRequestQueryString [("ref", Just $ encodeUtf8
              info.sha)]
          & HTTP.addRequestHeader "User-Agent" "FreerGit"
          & HTTP.addRequestHeader "Accept"
              "application/vnd.github.v3.raw" -- Only fetch raw file
  res <- HTTP.httpBS req
  Yaml.decodeThrow $ HTTP.getResponseBody res

createCloneStep :: JobHandler.CommitInfo -> Step
createCloneStep info = Step
  { name = StepName "clone"
  , commands = NonEmpty.Partial.fromList
    [ "git clone -q https://github.com/" <> info.repo <> " ."
    , "git checkout -qf " <> info.sha
    ]
  , image = Docker.Image "alpine/git" "v2.26.2"
  }