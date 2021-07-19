module JobHandler where

import RIO
import Core
import qualified Agent

data Job
    = Job
        { pipeline :: Pipeline
        , state :: JobState
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
        , repo :: Text
        }
    deriving (Eq, Show)

data Service 
    = Service 
        { queueJob :: Pipeline -> IO BuildNumber 
        , dispatchCmd :: IO (Maybe Agent.Cmd)
        , processMsg :: Agent.Msg -> IO ()
        , findJob :: BuildNumber -> IO (Maybe Job)
        , fetchLogs :: BuildNumber -> StepName -> IO (Maybe ByteString)
        }