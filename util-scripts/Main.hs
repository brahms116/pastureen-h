{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Abstract
import Control.Monad.Reader
import Service

main :: IO ()
main = runReaderT (runReaderT deploymentPipeline Local) defaultConfig
