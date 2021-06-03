{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Plutus.PAB.App(
    App,
    runApp,
    AppEnv(..),
    DbBackend(..),
    -- * App actions
    migrate,
    dbConnect,
    beamMigrate,
    beamDbConnect
    ) where

import           Cardano.BM.Trace                               (Trace)
import           Cardano.ChainIndex.Client                      (handleChainIndexClient)
import qualified Cardano.ChainIndex.Types                       as ChainIndex
import           Cardano.Node.Client                            (handleNodeClientClient)
import           Cardano.Node.Types                             (MockServerConfig (..))
import qualified Cardano.Protocol.Socket.Client                 as Client
import qualified Cardano.Wallet.Client                          as WalletClient
import qualified Cardano.Wallet.Types                           as Wallet
import qualified Control.Concurrent.STM                         as STM
import           Control.Monad                                  (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error                      (handleError, throwError)
import           Control.Monad.Freer.Extras.Log                 (mapLog)
import           Control.Monad.IO.Class                         (MonadIO (..))
import qualified Control.Monad.Logger                           as MonadLogger
import           Data.Coerce                                    (coerce)
import           Data.Text                                      (unpack)
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import qualified Database.Beam.Sqlite                           as Sqlite
import qualified Database.Beam.Sqlite.Migrate                   as Sqlite
import           Database.Persist.Sqlite                        (createSqlitePoolFromInfo, mkSqliteConnectionInfo,
                                                                 runSqlPool)
import           Database.SQLite.Simple                         (open)
import qualified Database.SQLite.Simple                         as Sqlite
import qualified Eventful.Store.Memory                          as M
import           Eventful.Store.Sqlite                          (defaultSqlEventStoreConfig, initializeSqliteEventStore)
import           Network.HTTP.Client                            (managerModifyRequest, newManager,
                                                                 setRequestIgnoreStatus)
import           Network.HTTP.Client.TLS                        (tlsManagerSettings)
import           Plutus.PAB.Core                                (EffectHandlers (..), PABAction)
import qualified Plutus.PAB.Core                                as Core
import qualified Plutus.PAB.Core.ContractInstance.BlockchainEnv as BlockchainEnv
import           Plutus.PAB.Core.ContractInstance.STM           as Instances
import qualified Plutus.PAB.Db.Eventful.ContractDefinitionStore as EventfulEff
import qualified Plutus.PAB.Db.Eventful.ContractStore           as EventfulEff
import           Plutus.PAB.Db.Memory.ContractStore             (InMemInstances, initialInMemInstances)
-- import qualified Plutus.PAB.Db.Memory.ContractDefinitionStore   as InMem
import qualified Plutus.PAB.Db.Beam.ContractDefinitionStore     as BeamEff
import qualified Plutus.PAB.Db.Beam.ContractStore               as BeamEff
import qualified Plutus.PAB.Db.Memory.ContractStore             as InMem
import           Plutus.PAB.Effects.Contract.ContractExe        (ContractExe, handleContractEffectContractExe)
import           Plutus.PAB.Effects.DbStore                     (Db, handleDbStore, initialSetupStep)
import           Plutus.PAB.Effects.EventLog                    (Connection (..), EventLogBackend (..), handleEventLog)
import qualified Plutus.PAB.Effects.EventLog                    as EventLog
import           Plutus.PAB.Events                              (PABEvent)
import           Plutus.PAB.Monitoring.MonadLoggerBridge        (TraceLoggerT (..))
import           Plutus.PAB.Monitoring.Monitoring               (convertLog, handleLogMsgTrace)
import           Plutus.PAB.Monitoring.PABLogMsg                (PABLogMsg (..))
import           Plutus.PAB.Timeout                             (Timeout (..))
import           Plutus.PAB.Types                               (Config (Config), DbConfig (..), PABError (..),
                                                                 chainIndexConfig, dbConfig, endpointTimeout,
                                                                 nodeServerConfig, walletServerConfig)
import           Servant.Client                                 (ClientEnv, mkClientEnv)

------------------------------------------------------------
data AppEnv =
    AppEnv
        { dbConnection          :: EventLogBackend (PABEvent ContractExe)
        , walletClientEnv       :: ClientEnv
        , nodeClientEnv         :: ClientEnv
        , chainIndexEnv         :: ClientEnv
        , txSendHandle          :: Client.TxSendHandle
        , chainSyncHandle       :: Client.ChainSyncHandle
        , appConfig             :: Config
        , appTrace              :: Trace IO (PABLogMsg ContractExe)
        , appInMemContractStore :: InMemInstances ContractExe
        }

appEffectHandlers :: DbBackend -> Config -> Trace IO (PABLogMsg ContractExe) -> EffectHandlers ContractExe AppEnv
appEffectHandlers dbBackend config trace =
    EffectHandlers
        { initialiseEnvironment = do
            env <- liftIO $ mkEnv dbBackend trace config
            let Config{nodeServerConfig=MockServerConfig{mscSocketPath, mscSlotConfig}} = config
            instancesState <- liftIO $ STM.atomically $ Instances.emptyInstancesState
            blockchainEnv <- liftIO $ BlockchainEnv.startNodeClient mscSocketPath mscSlotConfig
            pure (instancesState, blockchainEnv, env)

        , handleLogMessages =
            interpret (handleLogMsgTrace trace)
            . reinterpret (mapLog SMultiAgent)

        , handleContractEffect =
            interpret (handleLogMsgTrace trace)
            . reinterpret (mapLog @_ @(PABLogMsg ContractExe) SContractExeLogMsg)
            . reinterpret (handleContractEffectContractExe @IO)

        , handleContractStoreEffect =
            case dbBackend of
                BeamSqliteBackend ->
                  interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
                  . interpret (Core.handleMappedReader @AppEnv dbConnection)
                  . interpret (handleDbStore undefined)
                  . reinterpret3 BeamEff.handleContractStore

                EventfulInMemoryBackend ->
                    interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
                    . interpret (Core.handleMappedReader @AppEnv appInMemContractStore)
                    . reinterpret2 InMem.handleContractStore

                EventfulSqliteBackend ->
                    interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
                    . interpret (Core.handleMappedReader @AppEnv dbConnection)
                    . interpret (handleEventLog @_ @(PABEvent ContractExe) (convertLog SLoggerBridge trace))
                    . reinterpret3 EventfulEff.handleContractStore

        , handleContractDefinitionStoreEffect =
            case dbBackend of
                BeamSqliteBackend ->
                  interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
                  . interpret (Core.handleMappedReader @AppEnv dbConnection)
                  . interpret (handleDbStore undefined)
                  . reinterpret3 BeamEff.handleContractDefinitionStore

                EventfulInMemoryBackend ->
                  -- TODO: Implement
                  error "Not supported!!"
                --     interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
                --     . interpret (Core.handleMappedReader @AppEnv appInMemContractStore)
                --     . reinterpret2 InMem.handleContractDefinitionStore

                EventfulSqliteBackend ->
                    interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
                    . interpret (Core.handleMappedReader @AppEnv dbConnection)
                    . interpret (handleEventLog @_ @(PABEvent ContractExe) (convertLog SLoggerBridge trace))
                    . reinterpret3 EventfulEff.handleContractDefinitionStore

        , handleServicesEffects = \wallet ->

            -- handle 'NodeClientEffect'
            flip handleError (throwError . NodeClientError)
            . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . reinterpret (Core.handleMappedReader @AppEnv @Client.ChainSyncHandle chainSyncHandle)
            . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . reinterpret (Core.handleMappedReader @AppEnv @Client.TxSendHandle txSendHandle)
            . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . reinterpret (Core.handleMappedReader @AppEnv @ClientEnv nodeClientEnv)
            . reinterpretN @'[_, _, _, _] (handleNodeClientClient @IO)

            -- handle 'ChainIndexEffect'
            . flip handleError (throwError . ChainIndexError)
            . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . reinterpret (Core.handleMappedReader @AppEnv @ClientEnv chainIndexEnv)
            . reinterpret2 (handleChainIndexClient @IO)

            -- handle 'WalletEffect'
            . flip handleError (throwError . WalletClientError)
            . flip handleError (throwError . WalletError)
            . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . reinterpret (Core.handleMappedReader @AppEnv @ClientEnv walletClientEnv)
            . reinterpretN @'[_, _, _] (WalletClient.handleWalletClient @IO wallet)

        , onStartup = pure ()

        , onShutdown = pure ()
        }

runApp ::
    forall a.
    DbBackend
    -> Trace IO (PABLogMsg ContractExe) -- ^ Top-level tracer
    -> Config -- ^ Client configuration
    -> App a -- ^ Action
    -> IO (Either PABError a)
runApp dbBackend trace config@Config{endpointTimeout} = Core.runPAB (Timeout endpointTimeout) (appEffectHandlers dbBackend config trace)

type App a = PABAction ContractExe AppEnv a

data DbBackend
  = EventfulSqliteBackend
  | EventfulInMemoryBackend
  | BeamSqliteBackend
  deriving (Eq, Show, Read)

mkEnv :: DbBackend -> Trace IO (PABLogMsg ContractExe) -> Config -> IO AppEnv
mkEnv dbBackend appTrace appConfig@Config { dbConfig
             , nodeServerConfig =  MockServerConfig{mscBaseUrl, mscSocketPath, mscSlotConfig}
             , walletServerConfig
             , chainIndexConfig
             } = do
    walletClientEnv <- clientEnv (Wallet.baseUrl walletServerConfig)
    nodeClientEnv <- clientEnv mscBaseUrl
    chainIndexEnv <- clientEnv (ChainIndex.ciBaseUrl chainIndexConfig)
    dbConnection <- case dbBackend of
        EventfulSqliteBackend   -> Sqlite <$> dbConnect appTrace dbConfig
        EventfulInMemoryBackend -> InMemory <$> M.eventMapTVar
        -- TODO: Implement
        BeamSqliteBackend       -> error "!!!"

    txSendHandle <- liftIO $ Client.runTxSender mscSocketPath
    -- This is for access to the slot number in the interpreter
    chainSyncHandle <- liftIO $ Client.runChainSync' mscSocketPath mscSlotConfig
    appInMemContractStore <- liftIO initialInMemInstances
    pure AppEnv {..}
  where
    clientEnv baseUrl = mkClientEnv <$> liftIO mkManager <*> pure (coerce baseUrl)

    mkManager =
        newManager $
        tlsManagerSettings {managerModifyRequest = pure . setRequestIgnoreStatus}


beamMigrate :: Trace IO (PABLogMsg ContractExe) -> DbConfig -> IO ()
beamMigrate trace config = do
    connection <- beamDbConnect trace config
    flip runTraceLoggerT (convertLog SLoggerBridge trace) $ do
      MonadLogger.logDebugN "Running beam migration"
      liftIO
        $ void
        $ migrateDB connection


allowDestructive :: (MonadFail m) => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks
  { runIrreversibleHook = pure True }


-- TODO: Document
migrateDB
  :: Sqlite.Connection
  -> IO (Maybe (CheckedDatabaseSettings Sqlite.Sqlite Db))
migrateDB conn = Sqlite.runBeamSqliteDebug putStrLn conn $
  bringUpToDateWithHooks
    allowDestructive
    Sqlite.migrationBackend
    initialSetupStep


-- TODO: Document
beamDbConnect :: Trace IO (PABLogMsg ContractExe) -> DbConfig -> IO Sqlite.Connection
beamDbConnect trace DbConfig {dbConfigFile} =
  flip runTraceLoggerT (convertLog SLoggerBridge trace) $ do
    MonadLogger.logDebugN $ "Connecting to DB: " <> dbConfigFile
    liftIO $ open (unpack dbConfigFile)


-- | Initialize/update the database to hold events.
migrate :: Trace IO (PABLogMsg ContractExe) -> DbConfig -> IO ()
migrate trace config = do
    Connection (sqlConfig, connectionPool) <- dbConnect trace config
    flip runTraceLoggerT (convertLog SLoggerBridge trace) $ do
        liftIO
            $ flip runSqlPool connectionPool
            $ initializeSqliteEventStore sqlConfig connectionPool


------------------------------------------------------------
-- | Create a database 'Connection' containing the connection pool
-- plus some configuration information.
dbConnect :: Trace IO (PABLogMsg ContractExe) -> DbConfig -> IO EventLog.Connection
dbConnect trace DbConfig {dbConfigFile, dbConfigPoolSize} =
    flip runTraceLoggerT (convertLog SLoggerBridge trace) $ do
        let connectionInfo = mkSqliteConnectionInfo dbConfigFile
        MonadLogger.logDebugN $ "Connecting to DB: " <> dbConfigFile
        connectionPool <- createSqlitePoolFromInfo connectionInfo dbConfigPoolSize
        pure $ EventLog.Connection (defaultSqlEventStoreConfig, connectionPool)
