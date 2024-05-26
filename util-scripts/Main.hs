module Main (main) where

import Pipeline

main :: IO ()
main = runPipelineReal Local >> runPipelineReal Test
