{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.FHIR.Model
import RIO
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_hasFHIR
import System.IO

main :: IO ()
main = do
  print "test"
{-
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_hasFHIR.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
-}
