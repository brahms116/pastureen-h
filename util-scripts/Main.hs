{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Service
import Abstract
import Control.Monad.Reader

main :: IO ()
main = runReaderT deploymentPipeline Local 
