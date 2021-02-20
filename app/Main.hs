module Main where

import Prelude hiding (readFile, putStrLn)
import Options.Applicative
import Control.Monad
import System.Exit
import System.IO (stderr)
import Data.ByteString.Char8
import Data.ByteString.UTF8 (fromString)

import Paths_topos (version)
import qualified Data.Version as Version

data CompilerOpts = CompilerOpts
  { _showVersion :: Bool
  , _inputSource :: Maybe FilePath
  , _outputFileName :: Maybe FilePath
  }

pArg :: Parser CompilerOpts
pArg = CompilerOpts
  <$> switch (long "version" <> short 'v')
  <*> optional (strOption $ long "src"
                          <> help "Source file path"
                          <> metavar "FILEPATH"
                          <> short 'c'
  )
  <*> optional (strOption $ long "out"
                          <> help "Output file path"
                          <> metavar "OUTPATH"
                          <> short 'o'
  )

main :: IO ()
main = do
  flags <- customExecParser pref opts
  when (_showVersion flags) (printVersion >> exitSuccess)

  inputFilepath <- maybe inputFileNotFound pure (_inputSource flags)
  inputContent <- readFile inputFilepath

  print inputContent
  -- case Parser.parseSource inputFilepath inputContent of
  --   Left err -> error $ TL.pack err
  --   Right decls -> print decls

  where
    pref = prefs $ showHelpOnEmpty <> showHelpOnEmpty <> disambiguate <> columns 80

    opts = info (pArg <**> helper) $
      fullDesc
      <> progDesc "The only Topos compiler"
      <> footer "https://github.com/topoi-lang/topos"

    printVersion = putStrLn . fromString $ "Topos compiler " <> Version.showVersion version

    inputFileNotFound = do
      hPutStrLn stderr "Please specify an input file"
      exitFailure
