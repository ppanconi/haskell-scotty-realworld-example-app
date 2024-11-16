module Conduit.App.Env where

import Conduit.App.Has (Has', obtain)
import Conduit.DB.Core (DBPool)
import Conduit.Identity.JWT (JWTInfo)
import Data.Aeson (FromJSON)
import Colog (LogAction, Message, HasLog (..))

-- | The type of the runtime environment. @Development@ has more logging and auto-runs DB migrations.
data EnvType = Production | Development
  deriving (Show, Eq, Read, Generic, FromJSON)

-- | Global state of the application.
data Env m = Env
  { envDBPool  :: !DBPool  -- ^ The database connection pool.
  , envJWTInfo :: !JWTInfo -- ^ The necessary info for creating & verifying JWTs.
  , envType    :: !EnvType -- ^ The type of the runtime environment.
  , envLogAction :: !(LogAction m Message)
  }

logAction :: Env m -> LogAction m Message
logAction (Env _ _ _ la) = la

instance Has' DBPool (Env m) where
  obtain :: Env m -> DBPool
  obtain = (.envDBPool)
  {-# INLINE obtain #-}

instance Has' JWTInfo (Env m) where
  obtain :: Env m -> JWTInfo
  obtain = (.envJWTInfo)
  {-# INLINE obtain #-}

instance Has' EnvType (Env m) where
  obtain :: Env m -> EnvType
  obtain = (.envType)
  {-# INLINE obtain #-}

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = logAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newLogAction env = env { envLogAction = newLogAction }
    {-# INLINE setLogAction #-}