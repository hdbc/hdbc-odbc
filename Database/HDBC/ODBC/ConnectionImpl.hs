module Database.HDBC.ODBC.ConnectionImpl where

import qualified Database.HDBC.Statement as Types
import qualified Database.HDBC.Types as Types
import Database.HDBC.ColTypes as ColTypes
import Control.Exception (finally)

data Connection =
    Connection {
                getQueryInfo :: String -> IO ([SqlColDesc], [(String, SqlColDesc)]),
                disconnect :: IO (),
                commit :: IO (),
                rollback :: IO (),
                run :: String -> [Types.SqlValue] -> IO Integer,
                prepare :: String -> IO Types.Statement,
                clone :: IO Connection,
                hdbcDriverName :: String,
                hdbcClientVer :: String,
                proxiedClientName :: String,
                proxiedClientVer :: String,
                dbServerVer :: String,
                dbTransactionSupport :: Bool,
                getTables :: IO [String],
                describeTable :: String -> IO [(String, ColTypes.SqlColDesc)],
                -- | Changes AutoCommit mode of the given connection. Returns previous value.
                setAutoCommit :: Bool -> IO Bool
               }

instance Types.IConnection Connection where
  disconnect = disconnect
  commit = commit
  rollback = rollback
  run = run
  runRaw conn sql = do
    sth <- prepare conn sql
    Types.executeRaw sth `finally` Types.finish sth
  prepare = prepare
  clone = clone
  hdbcDriverName = hdbcDriverName
  hdbcClientVer = hdbcClientVer
  proxiedClientName = proxiedClientName
  proxiedClientVer = proxiedClientVer
  dbServerVer = dbServerVer
  dbTransactionSupport = dbTransactionSupport
  getTables = getTables
  describeTable = describeTable
