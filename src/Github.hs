{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Github where

import RIO
import Core
import Data.Aeson.Schema

import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Simple as HTTP
import qualified RIO.NonEmpty.Partial as NE
import qualified RIO.Text as Text
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.List.Partial as List
import qualified JobHandler
import qualified Docker


type CommitSchema = [schema|
  {
    repo: {
      name: Text,
    },
    payload: {
      ref: Maybe Text,
      head: Maybe Text,
      commits: Maybe List {
        sha: Text,
        author: {
          name: Text,
        },
        message: Text,
      },
    },
  }
|]

-- Note to self: I use lazy BS here because i thought it would speed up the parsing
-- since i only want the first element either way, there seems to be a slight difference
-- but not noticable from my tests. Forcing Aeson to only do @one@ successful parse 
-- would be perfect, unsure how though.
parsePushEvent :: LBS.ByteString -> IO JobHandler.CommitInfo    
parsePushEvent bs = do 
  body <- case Aeson.eitherDecode bs :: Either String [Object CommitSchema] of
    Left _ -> fail "No commits"
    Right x -> return x
  let info = List.head [get| body[] |] 
  let firstCommit =  List.head [get| info.payload.commits! |]
  pure JobHandler.CommitInfo
                  { sha = fromMaybe "" [get| info.payload.head|]
                  , repo = [get| info.repo.name|]
                  , branch = Text.dropPrefix "refs/heads/" (fromMaybe "" [get| info.payload.ref|])
                  , message = [get| firstCommit.message|]
                  , author = [get| firstCommit.author.name|]
                  }

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
  , commands = NE.fromList
    [ "git clone -q https://github.com/" <> info.repo <> " ."
    , "git checkout -qf " <> info.sha
    ]
  , image = Docker.Image "alpine/git" "v2.26.2"
  }
