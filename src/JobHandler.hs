module JobHandler where

import RIO
import Core
import qualified Agent
import qualified Data.Aeson as Aeson

data Job
    = Job
        { pipeline :: Pipeline
        , state :: JobState
        , info :: CommitInfo
        }
    deriving (Eq, Show)

data JobState
    = JobQueued
    | JobAssigned
    | JobScheduled Build
    deriving (Eq, Show)

data CommitInfo
    = CommitInfo
        { sha :: Text
        , branch :: Text
        , message :: Text
        , author :: Text
        , repo :: Text
        }
    deriving (Eq, Show, Generic, Aeson.ToJSON)

data Service 
    = Service 
        { queueJob :: CommitInfo -> Pipeline -> IO BuildNumber
        , findJob :: BuildNumber -> IO (Maybe Job)
        , dispatchCmd :: IO (Maybe Agent.Cmd)
        , processMsg :: Agent.Msg -> IO ()
        , fetchLogs :: BuildNumber -> StepName -> IO (Maybe ByteString)
        , latestJobs :: IO [(BuildNumber, Job)]
        }