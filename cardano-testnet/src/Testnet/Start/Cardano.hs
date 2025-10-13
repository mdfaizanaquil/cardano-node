{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Testnet.Start.Cardano
  ( CardanoTestnetCliOptions(..)
  , CardanoTestnetOptions(..)
  , NodeOption(..)
  , cardanoDefaultTestnetNodeOptions

  , TestnetRuntime (..)

  , cardanoTestnet
  , createAndRunTestnet
  , createTestnetEnv
  , getDefaultAlonzoGenesis
  , getDefaultShelleyGenesis
  , retryOnAddressInUseError
    -- Move to my own module
  , liftToIntegration
  , reconcileMaps
  ) where


import           Cardano.Api
import           Cardano.Api.Byron (GenesisData (..))
import qualified Cardano.Api.Byron as Byron

import           Cardano.Node.Configuration.Topology (RemoteAddress(..))
import qualified Cardano.Node.Configuration.Topology as Direct
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Cardano.Prelude (canonicalEncodePretty)
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint(..))

import           Prelude hiding (lines)

import           Control.Concurrent (threadDelay)
import           Control.Exception (Exception (..))
import           Control.Monad
import           Control.Monad.Trans.Resource.Internal (ReleaseMap(..))
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.KeyMap as A
import           Data.Acquire.Internal (ReleaseType)
import qualified Data.ByteString.Lazy as LBS
import           Data.Default.Class (def)
import           Data.Either
import           Data.Functor
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.MonoTraversable (Element, MonoFunctor, omap)
import qualified Data.Text as Text
import           Data.Time (diffUTCTime)
import           Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as DTC
import           GHC.Stack
import qualified System.Directory as IO
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import qualified Testnet.Defaults as Defaults
import           Testnet.Filepath
import           Testnet.Handlers (interruptNodesOnSigINT)
import           Testnet.Process.NewRun (execCli', execCli_, mkExecConfig)
import           Testnet.Property.Assert (assertChainExtended, assertExpectedSposInLedgerState)
import           Testnet.Runtime as TR
import           Testnet.Start.Types
import           Testnet.Types as TR hiding (shelleyGenesis)

import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Port as H


import RIO (RIO(..),runRIO, IORef, readIORef, modifyIORef', throwM)
import Control.Monad.Trans.Resource (ResourceT, getInternalState)
import Testnet.Orphans ()
import RIO.Orphans (HasResourceMap, withResourceMap)
import Control.Monad.IO.Unlift


newtype MinimumConfigRequirementsError
  = MinimumConfigRequirementsError String
  deriving Show

instance Exception MinimumConfigRequirementsError where
  displayException (MinimumConfigRequirementsError msg) = msg

-- | There are certain conditions that need to be met in order to run
-- a valid node cluster.
testMinimumConfigurationRequirements :: ()
  => HasCallStack
  => CardanoTestnetOptions -> RIO env ()
testMinimumConfigurationRequirements options = withFrozenCallStack $ do
  when (cardanoNumPools options < 1) $ do
    throwM $ MinimumConfigRequirementsError "Need at least one SPO node to produce blocks, but got none."

liftToIntegration :: IORef ReleaseMap -> RIO (IORef ReleaseMap) a -> H.Integration a 
liftToIntegration m r = lift . lift $ updateRIOState m r


-- Why do we need to do this? What happens if an IO action fails
-- when running the RIO monad? We need to clean up everthing else.
-- TODO: Accomodate for the IntegrationState as well!
updateRIOState
  :: MonadUnliftIO m
  => IORef ReleaseMap -- ^ External resource map
  -> RIO (IORef ReleaseMap) a 
  -> ResourceT m a
updateRIOState externalResourceMap rioAction = 
    withResourceMap 
      (\rioActionResoureMap -> do 
          liftIO $ appendResourceMap externalResourceMap rioActionResoureMap
          updatedState <- getInternalState
          runRIO updatedState rioAction
      )
 where 
  -- TODO: Figure out how to do this!
  appendResourceMap :: IORef ReleaseMap -> IORef ReleaseMap -> IO ()
  appendResourceMap externalMap internalMap = do
    extMap <- readIORef externalMap 
    intMap <- readIORef internalMap  
    modifyIORef' internalMap $ const $ appendResourceMapPure extMap intMap  


appendResourceMapPure :: ReleaseMap -> ReleaseMap -> ReleaseMap
appendResourceMapPure (ReleaseMap _ rf1 m1) (ReleaseMap _ rf2 m2) = 
    let finalMap = reconcileMaps m1 m2
    in ReleaseMap (getNextKey finalMap) (rf1 + rf2) finalMap
appendResourceMapPure _ _ = error "failed"

{-
createInternalState :: MonadIO m => m InternalState
createInternalState = liftIO
                    $ I.newIORef
                    $ ReleaseMap maxBound (minBound + 1) IntMap.empty
register' :: I.IORef ReleaseMap
          -> IO ()
          -> IO ReleaseKey
register' istate rel = I.atomicModifyIORef istate $ \rm ->
    case rm of
        ReleaseMap key rf m ->
            ( ReleaseMap (key - 1) rf (IntMap.insert key (const rel) m)
            , ReleaseKey istate key
            )
        ReleaseMapClosed -> throw $ InvalidAccess "register'"

-}

getNextKey :: IntMap (ReleaseType -> IO ()) -> Int
getNextKey m = case IntMap.lookupMin m of 
                Just (mini,_) -> mini - 1
                Nothing -> maxBound

reconcileMaps :: IntMap a-> IntMap a -> IntMap a
reconcileMaps externalMap internalMap = 
  let externalValues = IntMap.elems externalMap 
      internalValues = IntMap.elems internalMap
      finalValues = externalValues ++ internalValues
  in IntMap.fromList $ zip (dec maxBound) finalValues
 where 
   dec 0 = []
   dec start = 
     start : dec (start - 1)

-- TODO: Left off here
-- You are already decrementing the key. Therefore get the mod of each key
-- add them and then add the negative
-- The reference account appears to stay the same when registering actions
-- You need to generate the next key which is the negative of the sum of both keys plus 1 
-- You need to merge the maps. However you need to reassign the keys in the RIO internal map. 
-- You need to start from the most negative key in the external map and decrement it for each key in the internal 
-- RIO map. 


{-
register' :: I.IORef ReleaseMap
          -> IO ()
          -> IO ReleaseKey
register' istate rel = I.atomicModifyIORef istate $ \rm ->
    case rm of
        ReleaseMap key rf m ->
            -- First value in tuple is important 

            ( ReleaseMap (key - 1) rf (IntMap.insert key (const rel) m)
            , ReleaseKey istate key
            )
        ReleaseMapClosed -> throw $ InvalidAccess "register'"

-}


createTestnetEnv :: ()
  => HasCallStack
  => CardanoTestnetOptions
  -> GenesisOptions
  -> CreateEnvOptions
  -> Conf
  -> RIO e ()
createTestnetEnv
  testnetOptions@CardanoTestnetOptions
    { cardanoNodeEra=asbe
    , cardanoNodes
    }
  genesisOptions
  CreateEnvOptions
    { ceoOnChainParams=onChainParams
    , ceoTopologyType=topologyType
    }
  Conf
    { genesisHashesPolicy
    , tempAbsPath=TmpAbsolutePath tmpAbsPath
    } = do

  testMinimumConfigurationRequirements testnetOptions

  AnyShelleyBasedEra sbe <- pure asbe

  _ <- createSPOGenesisAndFiles
    testnetOptions genesisOptions onChainParams
    (TmpAbsolutePath tmpAbsPath)

  let configurationFile = tmpAbsPath </> "configuration.yaml"
  -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
  config' <- case genesisHashesPolicy of
    WithHashes -> createConfigJson (TmpAbsolutePath tmpAbsPath) sbe
    WithoutHashes -> pure $ createConfigJsonNoHash sbe
  -- Setup P2P configuration value
  let config = A.insert
        "EnableP2P"
        (Bool $ topologyType == P2PTopology)
        config'

  liftIO . LBS.writeFile configurationFile $ A.encodePretty $ Object config

  -- Create network topology, with abstract IDs in lieu of addresses
  let nodeIds = fst <$> zip [1..] cardanoNodes
  forM_ nodeIds $ \i -> do
    let nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
    liftIO $ IO.createDirectoryIfMissing True nodeDataDir

    let producers = NodeId <$> filter (/= i) nodeIds
    case topologyType of
      DirectTopology ->
        let topology = Direct.RealNodeTopology producers
        in liftIO . LBS.writeFile (nodeDataDir </> "topology.json") $ A.encodePretty topology
      P2PTopology ->
        let topology = Defaults.defaultP2PTopology producers
        in liftIO . LBS.writeFile (nodeDataDir </> "topology.json") $ A.encodePretty topology

-- | Starts a number of nodes, as configured by the value of the 'cardanoNodes'
-- field in the 'CardanoTestnetOptions' argument. Regarding this field, you can either:
--
-- 1. Pass a value 'UserProvidedNodeOptions filepath' to specify your own node configuration file.
--    In this case, only 1 node will be started (TODO: allow an arbitrary number of nodes to be started)
-- 2. Pass value 'NoUserProvidedData' to leave this function to generate the node configuration file.
--    In this, one SPO node will be started, as well as two relay nodes.
--
-- No matter the scenario above, this function setups a number of credentials and nodes (SPOs and relays), like this:
--
-- > ├── byron-gen-command
-- > │   └── genesis-keys.00{0,1,2}.key
-- > ├── delegate-keys
-- > │   ├── delegate{1,2,3}
-- > │   │   ├── kes.{skey,vkey}
-- > │   │   ├── key.{skey,vkey}
-- > │   │   ├── opcert.{cert,counter}
-- > │   │   └── vrf.{skey,vkey}
-- > │   └── README.md
-- > ├── drep-keys
-- > │   ├── drep{1,2,3}
-- > │   │   └── drep.{skey,vkey}
-- > │   └── README.md
-- > ├── genesis-keys
-- > │   ├── genesis{1,2,3}
-- > │   │   └── key.{skey,vkey}
-- > │   └── README.md
-- > ├── logs
-- > │   ├── node{1,2,3}
-- > │   │   ├── node.pid
-- > |   |   └── {stderr,stdout}.log
-- > │   ├── ledger-epoch-state-diffs.log
-- > │   ├── ledger-epoch-state.log
-- > │   ├── node-20241010121635.log
-- > │   └── node.log -> node-20241010121635.log
-- > ├── node-data
-- > │   ├── node{1,2,3}
-- > │   │   ├── db
-- > │   │   │   └── <node database files>
-- > │   │   ├── port
-- > │   │   └── topology.json
-- > ├── pools-keys
-- > │   ├── pool1
-- > │   │   ├── byron-delegate.key
-- > │   │   ├── byron-delegation.cert
-- > │   │   ├── cold.{skey,vkey}
-- > │   │   ├── kes.{skey,vkey}
-- > │   │   ├── opcert.{cert,counter}
-- > │   │   ├── staking-reward.{skey,vkey}
-- > │   │   └── vrf.{skey,vkey}
-- > │   └── README.md
-- > ├── socket
-- > │   ├── node{1,2,3}
-- > │   │   └── sock
-- > ├── stake-delegators
-- > │   ├── delegator{1,2,3}
-- > │   │   ├── payment.{skey,vkey}
-- > │   │   └── staking.{skey,vkey}
-- > ├── utxo-keys
-- > │   ├── utxo{1,2,3}
-- > │   │   └── utxo.{addr,skey,vkey}
-- > │   └── README.md
-- > ├── {alonzo,byron,conway,shelley}-genesis.json
-- > ├── configuration.json
-- > ├── current-stake-pools.json
-- > └── module
cardanoTestnet :: HasCallStack
  => HasResourceMap env
  => CardanoTestnetOptions -- ^ The options to use
  -> Conf -- ^ Path to the test sandbox
  -> RIO env TestnetRuntime
cardanoTestnet
  testnetOptions
  Conf
    { tempAbsPath=TmpAbsolutePath tmpAbsPath
    , updateTimestamps
    } = do
  let CardanoTestnetOptions
        { cardanoNodeLoggingFormat=nodeLoggingFormat
        , cardanoEnableNewEpochStateLogging=enableNewEpochStateLogging
        , cardanoNodes
        } = testnetOptions
      nPools = cardanoNumPools testnetOptions
      nodeConfigFile = tmpAbsPath </> "configuration.yaml"
      byronGenesisFile = tmpAbsPath </> "byron-genesis.json"
      shelleyGenesisFile = tmpAbsPath </> "shelley-genesis.json"

  --TODO: Remove H.note_ OS.os
  sBytes <- liftIO (LBS.readFile shelleyGenesisFile)
  shelleyGenesis@ShelleyGenesis{sgNetworkMagic} 
    <- case eitherDecode sBytes of 
          Right sg -> return sg
          Left err -> error $ "Could not decode shelley genesis file: " <> shelleyGenesisFile <> " Error: " <> err
  let testnetMagic :: Int = fromIntegral sgNetworkMagic

  wallets <- forM [1..3] $ \idx -> do
    let utxoKeys@KeyPair{verificationKey} = makePathsAbsolute $ Defaults.defaultUtxoKeys idx
    let paymentAddrFile = tmpAbsPath </> "utxo-keys" </> "utxo" <> show idx </> "utxo.addr"

    execCli_
      [ "latest", "address", "build"
      , "--payment-verification-key-file", unFile verificationKey
      , "--testnet-magic", show testnetMagic
      , "--out-file", paymentAddrFile
      ]

    paymentAddr <- liftIO $ readFile paymentAddrFile

    pure $ PaymentKeyInfo
      { paymentKeyInfoPair = utxoKeys
      , paymentKeyInfoAddr = Text.pack paymentAddr
      }

  portNumbersWithNodeOptions <- forM cardanoNodes
    (\nodeOption -> (nodeOption,) <$> H.randomPort testnetDefaultIpv4Address)

  let portNumbers = zip [1..] $ snd <$> portNumbersWithNodeOptions

  forM_ portNumbers $ \(i, portNumber) -> do
    let nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
    liftIO $ IO.createDirectoryIfMissing True nodeDataDir
    liftIO $ writeFile (nodeDataDir </> "port") (show portNumber)

  let
      idToRemoteAddressDirect :: ()
        => HasCallStack
        => NodeId -> RIO env RemoteAddress
      idToRemoteAddressDirect (NodeId i) = case lookup i portNumbers of
        Just port -> pure $ RemoteAddress
          { raAddress = showIpv4Address testnetDefaultIpv4Address
          , raPort = port
          , raValency = 1
          }
        Nothing -> do
          error $ "Found node id that was unaccounted for: " ++ show i
         -- H.note_ $ "Found node id that was unaccounted for: " ++ show i
        --  H.failure
      idToRemoteAddressP2P :: ()
        => HasCallStack
        => NodeId -> RIO env RelayAccessPoint
      idToRemoteAddressP2P (NodeId i) = case lookup i portNumbers of
        Just port -> pure $ RelayAccessAddress
            (showIpv4Address testnetDefaultIpv4Address)
            port
        Nothing -> do
          error $ "Found node id that was unaccounted for: " ++ show i
          -- H.note_ $ "Found node id that was unaccounted for: " ++ show i
          -- H.failure
  -- error "mid cardanoTestnet" works
  -- Implement concrete topology from abstract one, if necessary
  forM_ portNumbers $ \(i, _port) -> do
    let topologyPath = tmpAbsPath </> Defaults.defaultNodeDataDir i </> "topology.json"

    -- Try to decode either a direct topology file, or a P2P one
    tBytes <- liftIO $ LBS.readFile topologyPath
    case eitherDecode tBytes of
      Right (abstractTopology :: Direct.NetworkTopology NodeId) -> do
        topology <- mapM idToRemoteAddressDirect abstractTopology
        liftIO . LBS.writeFile topologyPath $ encode topology
      Left _ ->
        case eitherDecode tBytes of
          Right (abstractTopology :: P2P.NetworkTopology NodeId) -> do
            topology <- mapM idToRemoteAddressP2P abstractTopology
            liftIO . LBS.writeFile topologyPath $ encode topology
          Left e ->
            -- There can be multiple reasons for why both decodings have failed.
            -- Here we assume, very optimistically, that the user has already
            -- instantiated it with a concrete topology file.
            error $ "Could not decode topology file: " <> topologyPath <> ". This may be okay. Reason for decoding failure is:\n" ++ e
           -- H.note_ $ "Could not decode topology file. This may be okay. Reason for decoding failure is:\n" ++ e

  -- If necessary, update the time stamps in Byron and Shelley Genesis files.
  -- This is a QoL feature so that users who edit their configuration files don't
  -- have to manually set up the start times themselves.
  when (updateTimestamps == UpdateTimestamps) $ do
    currentTime <- liftIO DTC.getCurrentTime
    let startTime = DTC.addUTCTime startTimeOffsetSeconds currentTime

    -- Update start time in Byron genesis file
    eByron <- runExceptT $ Byron.readGenesisData byronGenesisFile
    (byronGenesis', _byronHash) <- 
      case eByron of 
        Right bg -> return bg
        Left err -> error $ "Could not read byron genesis data from file: " <> byronGenesisFile <> " Error: " <> show err
    let byronGenesis = byronGenesis'{gdStartTime = startTime}
    liftIO . LBS.writeFile  byronGenesisFile $ canonicalEncodePretty byronGenesis

    -- Update start time in Shelley genesis file (which has been read already)
    let shelleyGenesis' = shelleyGenesis{sgSystemStart = startTime}
    liftIO . LBS.writeFile shelleyGenesisFile $ A.encodePretty shelleyGenesis'
  -- error "mid cardanoTestnet 2"  works
  eTestnetNodes <- H.forConcurrently (zip [1..] portNumbersWithNodeOptions) $ \(i, (nodeOptions, port)) -> do
    let nodeName = Defaults.defaultNodeName i
        nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
        nodePoolKeysDir = tmpAbsPath </> Defaults.defaultSpoKeysDir i
   -- H.note_ $ "Node name: " <> nodeName
    let (mKeys, spoNodeCliArgs) =
          case nodeOptions of
            RelayNodeOptions{} -> (Nothing, [])
            SpoNodeOptions{} -> (Just keys, shelleyCliArgs <> byronCliArgs)
          where
            shelleyCliArgs = [ "--shelley-kes-key", nodePoolKeysDir </> "kes.skey"
                             , "--shelley-vrf-key", unFile $ signingKey poolNodeKeysVrf
                             , "--shelley-operational-certificate", nodePoolKeysDir </> "opcert.cert"
                             ]
            byronCliArgs = [ "--byron-delegation-certificate", nodePoolKeysDir </> "byron-delegation.cert"
                           , "--byron-signing-key", nodePoolKeysDir </> "byron-delegate.key"
                           ]
            keys@SpoNodeKeys{poolNodeKeysVrf} = mkTestnetNodeKeyPaths i
    -- error "forCuncurrently" -- works!
    eRuntime <- runExceptT . retryOnAddressInUseError $
      startNode (TmpAbsolutePath tmpAbsPath) nodeName testnetDefaultIpv4Address port testnetMagic $
        [ "run"
        , "--config", nodeConfigFile
        , "--topology", nodeDataDir </> "topology.json"
        , "--database-path", nodeDataDir </> "db"
        ]
        <> spoNodeCliArgs
        <> extraCliArgs nodeOptions
    -- error "eRuntime" -- works!
    pure $ eRuntime <&> \rt -> rt{poolKeys=mKeys}
  -- error "mid cardanoTestnet before 3" -- works!! 
  let (failedNodes, testnetNodes') = partitionEithers eTestnetNodes
  unless (null failedNodes) $ do
    error $ "Some nodes failed to start:\n" ++ show (vsep (prettyError <$> failedNodes))
  --  H.noteShow_ . vsep $ prettyError <$> failedNodes
  --  H.failure

  -- Interrupt cardano nodes when the main process is interrupted
  liftIO $ interruptNodesOnSigINT testnetNodes'
  -- H.annotateShow $ nodeSprocket <$> testnetNodes'

  -- FIXME: use foldEpochState waiting for chain extensions
  now <- liftIO DTC.getCurrentTime
  let deadline = DTC.addUTCTime 45 now
  forM_ (map nodeStdout testnetNodes') $ \nodeStdoutFile -> do
    assertChainExtended deadline nodeLoggingFormat nodeStdoutFile

  -- H.noteShowIO_ DTC.getCurrentTime

  let runtime = TestnetRuntime
        { configurationFile = File nodeConfigFile
        , shelleyGenesisFile = tmpAbsPath </> Defaults.defaultGenesisFilepath ShelleyEra
        , testnetMagic
        , testnetNodes=testnetNodes'
        , wallets
        , delegators = []
        }

  let tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tmpAbsPath

  let node1sprocket = head $ testnetSprockets runtime
  execConfig <- mkExecConfig tempBaseAbsPath node1sprocket testnetMagic
  -- error "mid cardanoTestnet 3"  -- workks!
  forM_ wallets $ \wallet -> do
    -- H.cat . signingKeyFp $ paymentKeyInfoPair wallet
    -- H.cat . verificationKeyFp $ paymentKeyInfoPair wallet

    execCli' execConfig
    --- utxos <- execCli' execConfig
      [ "latest", "query", "utxo"
      , "--address", Text.unpack $ paymentKeyInfoAddr wallet
      , "--cardano-mode"
      ]

    -- H.note_ utxos

  let stakePoolsFp = tmpAbsPath </> "current-stake-pools.json"

  assertExpectedSposInLedgerState stakePoolsFp nPools execConfig

  when enableNewEpochStateLogging $
    TR.startLedgerNewEpochStateLogging runtime tempBaseAbsPath

  pure runtime
  where
    extraCliArgs = \case
      SpoNodeOptions args -> args
      RelayNodeOptions args -> args
    -- TODO: This should come from the configuration!
    makePathsAbsolute :: (Element a ~ FilePath, MonoFunctor a) => a -> a
    makePathsAbsolute = omap (tmpAbsPath </>)
    mkTestnetNodeKeyPaths :: Int -> SpoNodeKeys
    mkTestnetNodeKeyPaths n = makePathsAbsolute $ Defaults.defaultSpoKeys n

-- | A convenience wrapper around `createTestnetEnv` and `cardanoTestnet`
createAndRunTestnet :: ()
  => HasCallStack
  => CardanoTestnetOptions -- ^ The options to use
  -> GenesisOptions
  -> Conf -- ^ Path to the test sandbox
  -> H.Integration TestnetRuntime
createAndRunTestnet testnetOptions genesisOptions conf = do
  r <- lift $ lift getInternalState 
  liftToIntegration r $ do
     -- works error "here"
     createTestnetEnv
       testnetOptions genesisOptions def
       conf
     -- works error "here"
     cardanoTestnet testnetOptions conf
     -- error "here post cardanoTestnet" -- WORKS!

-- | Retry an action when `NodeAddressAlreadyInUseError` gets thrown from an action
retryOnAddressInUseError
  :: forall m a. HasCallStack
  => MonadIO m
  => ExceptT NodeStartFailure m a -- ^ action being retried
  -> ExceptT NodeStartFailure m a
retryOnAddressInUseError act = withFrozenCallStack $ go maximumTimeout retryTimeout
  where
    go :: HasCallStack => NominalDiffTime -> NominalDiffTime -> ExceptT NodeStartFailure m a
    go timeout interval
      | timeout <= 0 = withFrozenCallStack $ do
        --H.note_ "Exceeded timeout when retrying node start"
        act
      | otherwise = withFrozenCallStack $ do
        !time <- liftIO DTC.getCurrentTime
        catchError act $ \case
          NodeAddressAlreadyInUseError _ -> do
            liftIO $ threadDelay (round $ interval * 1_000_000)
            !time' <- liftIO DTC.getCurrentTime
            let elapsedTime = time' `diffUTCTime` time
                newTimeout = timeout - elapsedTime
          --  H.note_ $ "Retrying on 'address in use' error, timeout: " <> show newTimeout
            go newTimeout interval
          e -> throwError e

    -- Retry timeout in seconds. This should be > 2 * net.inet.tcp.msl on darwin,
    -- net.inet.tcp.msl in RFC 793 determines TIME_WAIT socket timeout.
    -- Usually it's 30 or 60 seconds. We take two times that plus some extra time.
    maximumTimeout = 150
    -- Wait for that many seconds before retrying.
    retryTimeout = 5
