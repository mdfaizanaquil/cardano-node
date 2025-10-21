
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Process.NewRun where

import           Prelude
import           Data.Aeson (eitherDecode)
import           Data.Monoid (Last (..))
import           Hedgehog.Extras.Internal.Plan (Component (..), Plan (..))
import           RIO
import           System.FilePath (takeDirectory)
import           System.FilePath.Posix ((</>))
import           System.Process (CreateProcess (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Text as T
import qualified GHC.Stack as GHC
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import           Hedgehog.Extras.Test.Process (ExecConfig(..))
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.Exit as IO
import qualified System.IO.Unsafe as IO
import qualified System.Process as IO


defaultExecConfig :: ExecConfig
defaultExecConfig = ExecConfig
  { execConfigEnv = mempty
  , execConfigCwd = mempty
  }


mkExecConfig :: ()
  => FilePath
  -> IO.Sprocket
  -> Int -- ^ Network id
  -> RIO env ExecConfig
mkExecConfig tempBaseAbsPath sprocket networkId = do
  env' <- liftIO IO.getEnvironment

  return ExecConfig
    { execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName sprocket)
      , ("CARDANO_NODE_NETWORK_ID", show networkId)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env'
    , execConfigCwd = Last $ Just tempBaseAbsPath
    }


execCli'
  :: HasCallStack
  => ExecConfig
  -> [String]
  -> RIO env String
execCli' execConfig = GHC.withFrozenCallStack $ execFlex' execConfig "cardano-cli" "CARDANO_CLI"

execCli_
  :: HasCallStack
  => MonadIO m
  => [String]
  -> m ()
execCli_ = GHC.withFrozenCallStack $ void . liftIO . runRIO () . execCli

execCli
  :: HasCallStack
  => [String]
  -> RIO env String
execCli = GHC.withFrozenCallStack $ execFlex "cardano-cli" "CARDANO_CLI"

-- | Create a process returning its stdout.
--
-- Being a 'flex' function means that the environment determines how the process is launched.
--
-- When running in a nix environment, the 'envBin' argument describes the environment variable
-- that defines the binary to use to launch the process.
--
-- When running outside a nix environment, the `pkgBin` describes the name of the binary
-- to launch via cabal exec.
execFlex
  :: HasCallStack
  => String
  -> String
  -> [String]
  -> RIO env String
execFlex = execFlex' defaultExecConfig

execFlex'
  :: HasCallStack
  => ExecConfig
  -> String
  -> String
  -> [String]
  -> RIO env String
execFlex' execConfig pkgBin envBin arguments = GHC.withFrozenCallStack $ do
  (exitResult, stdout', _stderr) <- execFlexAny' execConfig pkgBin envBin arguments
  case exitResult of
    f@(IO.ExitFailure _exitCode) -> throwM f
      -- H.annotate $ L.unlines $
      --   [ "Process exited with non-zero exit-code: " ++ show @Int exitCode ]
      --   ++ (if L.null stdout then [] else ["━━━━ stdout ━━━━" , stdout])
      --   ++ (if L.null stderr then [] else ["━━━━ stderr ━━━━" , stderr])
      -- H.failMessage GHC.callStack "Execute process failed"
    IO.ExitSuccess -> return stdout'

-- | Run a process, returning its exit code, its stdout, and its stderr.
-- Contrary to @execFlex'@, this function doesn't fail if the call fails.
-- So, if you want to test something negative, this is the function to use.
execFlexAny'
  :: HasCallStack
  => ExecConfig
  -> String -- ^ @pkgBin@: name of the binary to launch via 'cabal exec'
  -> String -- ^ @envBin@: environment variable defining the binary to launch the process, when in Nix
  -> [String]
  -> RIO env (ExitCode, String, String) -- ^ exit code, stdout, stderr
execFlexAny' execConfig pkgBin envBin arguments = GHC.withFrozenCallStack $ do
  cp <- procFlex' execConfig pkgBin envBin arguments
  --H.annotate . ("━━━━ command ━━━━\n" <>) $ case IO.cmdspec cp of
  --  IO.ShellCommand cmd -> cmd
  --  IO.RawCommand cmd args -> cmd <> " " <> L.unwords (argQuote <$> args)
  liftIO $ IO.readCreateProcessWithExitCode cp ""



procFlex'
  :: HasCallStack
  => ExecConfig
  -> String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> RIO env CreateProcess
  -- ^ Captured stdout
procFlex' execConfig pkg binaryEnv arguments = GHC.withFrozenCallStack $ do
  bin <- binFlex pkg binaryEnv
  return (IO.proc bin arguments)
    { IO.env = getLast $ execConfigEnv execConfig
    , IO.cwd = getLast $ execConfigCwd execConfig
    -- this allows sending signals to the created processes, without killing the test-suite process
    , IO.create_group = True
    }

-- | Compute the path to the binary given a package name or an environment variable override.
binFlex
  :: (HasCallStack)
  => String
  -- ^ Package name
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> RIO env FilePath
  -- ^ Path to executable
binFlex pkg binaryEnv = do
  maybeEnvBin <- liftIO $ IO.lookupEnv binaryEnv
  case maybeEnvBin of
    Just envBin -> return envBin
    Nothing -> binDist pkg binaryEnv

-- | Discover the location of the plan.json file.
planJsonFile :: String
planJsonFile = IO.unsafePerformIO $ do
  maybeBuildDir <- liftIO $ IO.lookupEnv "CABAL_BUILDDIR"
  case maybeBuildDir of
    Just buildDir -> return $ ".." </> buildDir </> "cache/plan.json"
    Nothing -> findDefaultPlanJsonFile
{-# NOINLINE planJsonFile #-}


-- | Find the nearest plan.json going upwards from the current directory.
findDefaultPlanJsonFile :: IO FilePath
findDefaultPlanJsonFile = IO.getCurrentDirectory >>= go
  where go :: FilePath -> IO FilePath
        go d = do
          let planRelPath = "dist-newstyle/cache/plan.json"
              file = d </> planRelPath
          exists <- IO.doesFileExist file
          if exists
            then return file
            else do
              let parent = takeDirectory d
              if parent == d
                then return planRelPath
                else go parent

addExeSuffix :: String -> String
addExeSuffix s = if ".exe" `L.isSuffixOf` s
  then s
  else s <> exeSuffix


exeSuffix :: String
exeSuffix = if OS.isWin32 then ".exe" else ""

-- | Consult the "plan.json" generated by cabal to get the path to the executable corresponding.
-- to a haskell package.  It is assumed that the project has already been configured and the
-- executable has been built.
-- Throws an exception on failure.
binDist
  :: HasCallStack
  => String
  -- ^ Package name
  -> String
  -- ^ Environment variable pointing to the binary to run (used for error messages only)
  -> RIO env FilePath
  -- ^ Path to executable
binDist pkg binaryEnv = do
  doesPlanExist <- liftIO $ IO.doesFileExist planJsonFile
  unless doesPlanExist $
    error $ "Could not find plan.json in the path: "
              <> planJsonFile
              <> ". Please run \"cabal build "
              <> pkg
              <> "\" if you are working with sources. Otherwise define "
              <> binaryEnv
              <> " and have it point to the executable you want."
  contents <- liftIO $ LBS.readFile planJsonFile

  case eitherDecode contents of
    Right plan -> case L.filter matching (plan & installPlan) of
      (component:_) -> case component & binFile of
        Just bin -> return $ addExeSuffix (T.unpack bin)
        Nothing -> error $ "missing \"bin-file\" key in plan component: " <> show component <> " in the plan in: " <> planJsonFile
      [] -> error $ "Cannot find \"component-name\" key with the value \"exe:" <> pkg <> "\" in the plan in: " <> planJsonFile
    Left message -> error $ "Cannot decode plan in " <> planJsonFile <> ": " <> message
  where matching :: Component -> Bool
        matching component = case componentName component of
          Just name -> name == "exe:" <> T.pack pkg
          Nothing -> False



procNode
  :: (HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> RIO env CreateProcess
  -- ^ Captured stdout
procNode = GHC.withFrozenCallStack $ procFlex "cardano-node" "CARDANO_NODE"


-- | Create a 'CreateProcess' describing how to start a process given the Cabal package name
-- corresponding to the executable, an environment variable pointing to the executable,
-- and an argument list.
--
-- The actual executable used will the one specified by the environment variable, but if
-- the environment variable is not defined, it will be found instead by consulting the
-- "plan.json" generated by cabal.  It is assumed that the project has already been
-- configured and the executable has been built.
procFlex
  :: HasCallStack
  => String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> RIO env CreateProcess
  -- ^ Captured stdout
procFlex = procFlex' defaultExecConfig