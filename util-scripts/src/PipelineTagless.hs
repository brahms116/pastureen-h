{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module PipelineTagless () where

import Control.Monad.Trans
import Control.Monad.Reader
import qualified System.Process as P

class HasInfraProjectDir m where
  getInfraProjectDir :: m FilePath
  default getInfraProjectDir :: (MonadReader FilePath m) => m FilePath
  getInfraProjectDir = ask

class HasTerraformProjectDir m where
  getTerraformProjectDir :: m FilePath
  default getTerraformProjectDir :: (MonadReader FilePath m) => m FilePath
  getTerraformProjectDir = ask

class DeployTerraform m where
  deployTerraform :: FilePath -> m ()
  default deployTerraform :: (MonadIO m) => FilePath -> m ()
  deployTerraform fp = do
    -- liftIO $ P.callCommand $ "cd " ++ fp ++ " && terraform init && terraform apply -auto-approve"
    liftIO $ putStrLn $ "cd " ++ fp ++ " && terraform init && terraform apply -auto-approve"

class DeployDb m where
  deployDb :: m ()
  default deployDb :: (Monad m, DeployTerraform m, HasInfraProjectDir m) => m ()
  deployDb = do
    d <- getInfraProjectDir
    deployTerraform d

newtype MyM a = MyM {unMyM :: IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving anyclass (DeployTerraform, DeployDb)

instance HasInfraProjectDir MyM where
  getInfraProjectDir = pure "infra"

deploy :: MyM ()
deploy = deployDb
