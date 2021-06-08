{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# options_ghc -Wno-missing-signatures #-}

module Plutus.PAB.Effects.DbStore where

import           Cardano.BM.Trace                        (Trace)
import           Control.Monad.Freer                     (Eff, LastMember, Member, type (~>))
import           Control.Monad.Freer.Reader              (Reader, ask)
import           Control.Monad.Freer.TH                  (makeEffect)
import qualified Control.Monad.Logger                    as MonadLogger
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate
import           Database.Beam.Schema.Tables
import           Database.Beam.Sqlite
import           Database.SQLite.Simple                  (Connection)
import           Plutus.PAB.Monitoring.MonadLoggerBridge (MonadLoggerMsg, TraceLoggerT (..))

data ContractT f
    = Contract
    { _contractPath :: Columnar f Text
    }
    deriving (Generic, Beamable)

-- TODO: How to give signatures for these functions? (or hide the warning?)
Contract (LensFor contractPath) = tableLenses

type Contract   = ContractT Identity
type ContractId = PrimaryKey ContractT Identity

instance Table ContractT where
  data PrimaryKey ContractT f = ContractId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = ContractId . _contractPath


data ContractInstanceT f
  = ContractInstance
    { _contractInstanceId           :: Columnar f Text
    , _contractInstanceContractPath :: Columnar f Text -- TODO: Foreign Key
    , _contractInstanceWallet       :: Columnar f Text -- Note/Sadness: Sqlite doesn't have a integer type large enough.
    , _contractInstanceState        :: Columnar f (Maybe Text)
    , _contractInstanceActive       :: Columnar f Bool
    } deriving (Generic, Beamable)

ContractInstance
  (LensFor contractInstanceId)
  (LensFor contractInstanceContractPath)
  (LensFor contractInstanceWallet)
  (LensFor contractInstanceState)
  (LensFor contractInstanceActive)
  = tableLenses


type ContractInstance   = ContractInstanceT Identity
type ContractInstanceId = PrimaryKey ContractInstanceT Identity

instance Table ContractInstanceT where
  data PrimaryKey ContractInstanceT f = ContractInstanceId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = ContractInstanceId . _contractInstanceId

data Db f = Db
    { _contracts         :: f (TableEntity ContractT)
    , _contractInstances :: f (TableEntity ContractInstanceT)
    }
    deriving (Generic, Database be)

db :: DatabaseSettings be Db
db = defaultDbSettings

-- TODO: Use `beam-automigrate`
initialSetupStep
  :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite Db)
initialSetupStep =
  migrationStep
  "initial_setup: ContractDefinitionStore"
  $ const
  $ Db <$>
       (createTable "contracts" $ Contract
        { _contractPath = field "path" (varchar Nothing) notNull unique }
       )
       <*>
       (createTable "instances" $ ContractInstance
        { _contractInstanceId           = field "instance_id"            (varchar Nothing) notNull unique
        , _contractInstanceWallet       = field "instance_wallet"        (varchar Nothing) notNull
        , _contractInstanceContractPath = field "instance_contract_path" (varchar Nothing) notNull
        , _contractInstanceState        = field "instance_state"         (maybeType characterLargeObject)
        , _contractInstanceActive       = field "instance_active"        boolean notNull
        }
       )

-- | Effect for managing a Db store.
data DbStoreEffect r where
  -- | Insert a row into a table.
  AddRow
    ::
    ( Beamable table
    , FieldsFulfillConstraint (BeamSqlBackendCanSerialize Sqlite) table
    )
    => DatabaseEntity Sqlite Db (TableEntity table)
    -> table Identity
    -> DbStoreEffect ()

  UpdateRow
    ::
    ( Beamable table
    )
    => SqlUpdate Sqlite table
    -> DbStoreEffect ()

  SelectList
    ::
    ( Beamable table
    , FromBackendRow Sqlite (table Identity)
    )
    => SqlSelect Sqlite (table Identity)
    -> DbStoreEffect [table Identity]

  SelectOne
    ::
    ( Beamable table
    , FromBackendRow Sqlite (table Identity)
    )
    => SqlSelect Sqlite (table Identity)
    -> DbStoreEffect (Maybe (table Identity))

handleDbStore ::
  forall effs.
  ( Member (Reader Connection) effs
  , LastMember IO effs
  )
  => Trace IO MonadLoggerMsg
  -> DbStoreEffect
  ~> Eff effs
handleDbStore trace eff = do
  connection <- ask @Connection
  let traceSql = flip runTraceLoggerT trace . MonadLogger.logDebugN . Text.pack

  case eff of
    AddRow table record ->
      liftIO
        $ runBeamSqliteDebug traceSql connection
        $ runInsert
        $ insert table (insertValues [record])

    SelectList q ->
      liftIO
        $ runBeamSqliteDebug traceSql connection
        $ runSelectReturningList q

    SelectOne q ->
      liftIO
        $ runBeamSqliteDebug traceSql connection
        $ runSelectReturningOne q

    UpdateRow q ->
      liftIO
        $ runBeamSqliteDebug traceSql connection
        $ runUpdate q

makeEffect ''DbStoreEffect
