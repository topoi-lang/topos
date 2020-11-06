-- | Program entry point lies here

module Main where
import Control.Applicative
import Control.Monad (when)
import Data.Either
import Options.Applicative
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess)
import Paths_topos (version)
import qualified Data.Version as Version

import qualified Data.ByteString as BS

import qualified Parser
import Value
import Type

data CompilerOpts = CompilerOpts
  { showVersion :: Bool
  , compilerInputFile :: Maybe FilePath
  , compilerOutputFile :: Maybe FilePath
  }

arg :: Parser CompilerOpts
arg = CompilerOpts
  <$> switch (long "version" <> short 'v')
  <*> optional
    (strOption $ long "src"
              <> help "Source file path"
              <> metavar "FILEPATH"
              <> short 'c'
              )
  <*> optional
    (strOption $ long "out"
              <> help "Output file path"
              <> metavar "OUT PATH"
              <> short 'o')

main :: IO ()
main = do
  flags <- customExecParser pref opts
  when (showVersion flags) (printVersion >> exitSuccess)

  inputFilepath <- maybe inputFileNotFound pure (compilerInputFile flags)
  inputContent <- BS.readFile inputFilepath

  case Parser.parseSrc inputFilepath inputContent of
    Left errorMsg -> error errorMsg
    Right term -> when (isRight $ typeCheck initEnv term) $
      print $ eval initScope term

  where
    pref = prefs $ 
      showHelpOnEmpty <> showHelpOnError <> disambiguate <> columns 80

    opts = info (arg <**> helper) $
      fullDesc
      <> progDesc "The only Topos compiler"
      <> footer "https://github.com/topoi-lang/topos"

printVersion :: IO ()
printVersion = putStrLn $ "Topos compiler " <> Version.showVersion version

inputFileNotFound :: IO a
inputFileNotFound = do
  hPutStrLn stderr "Please specify an input file."
  exitFailure
