{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Application
import Control.Monad.Reader
import Pipeline

main :: IO ()
main =
  let env = Local
   in runReaderT deploymentPipeline env
