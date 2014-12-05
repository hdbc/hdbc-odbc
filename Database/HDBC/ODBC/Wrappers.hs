{-| Defines haskell wrappers over native ODBC objects to free
    you from some memory (and threading) management.
-}
{-# LANGUAGE OverloadedStrings #-}
module Database.HDBC.ODBC.Wrappers
  ( EnvWrapper (..)
  , sqlAllocEnv
  , withMaybeEnv
  , withEnvOrDie
  , freeEnvIfNotAlready
  , DbcWrapper (..)
  , sqlAllocDbc
  , withMaybeDbc
  , withDbcOrDie
  , tryDisconnectAndFree
  , freeDbcIfNotAlready
  , StmtWrapper (..)
  , sqlAllocStmt
  , withMaybeStmt
  , withStmtOrDie
  , freeStmtIfNotAlready
  ) where

import Control.Monad (unless, void, when)
import Control.Concurrent.MVar
import Control.Concurrent.ReadWriteVar (RWVar)
import Database.HDBC (SqlError (..), throwSqlError)
import Database.HDBC.ODBC.Api.Errors
import Database.HDBC.ODBC.Api.Imports
import Database.HDBC.ODBC.Api.Types
import Database.HDBC.ODBC.Log
import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable
import System.Mem.Weak (addFinalizer)

import qualified Control.Concurrent.ReadWriteVar as RWV
import qualified Data.Foldable as F

data EnvWrapper = EnvWrapper
  { envHandle :: MVar (Maybe SQLHENV) -- ^ If there is Nothing here, environment is already freed
  }

sqlAllocEnv :: IO EnvWrapper
sqlAllocEnv = do
    hEnv <- alloca $ \(penvptr :: Ptr SQLHENV) -> do
      retVal <- c_sqlAllocHandle sQL_HANDLE_ENV nullPtr (castPtr penvptr)
      unless (sqlSucceeded retVal) $ throwSqlError SqlError
        { seState = ""
        , seNativeError = -1
        , seErrorMsg = "sqlAllocEnv/SqlAllocHandle: Failed to allocate ODBC Environment handle"
        }
      peek penvptr

    handleVar <- newMVar (Just hEnv)
    let wrapper = EnvWrapper handleVar

    addFinalizer wrapper $ freeEnvIfNotAlready wrapper
    return wrapper

freeEnvIfNotAlready :: EnvWrapper -> IO ()
freeEnvIfNotAlready w = modifyMVar_ (envHandle w) $ \maybeEnv -> do
  F.forM_ maybeEnv $ \hEnv -> do
    hdbcTrace $ "Freeing environment with handle " ++ show hEnv
    void $ c_sqlFreeHandle sQL_HANDLE_ENV (castPtr hEnv)
  return Nothing

withMaybeEnv :: EnvWrapper -> (Maybe SQLHENV -> IO a) -> IO a
withMaybeEnv = withMVar . envHandle

withEnvOrDie :: EnvWrapper -> (SQLHENV -> IO a) -> IO a
withEnvOrDie ew act = withMaybeEnv ew $ \maybeHandle ->
  case maybeHandle of
    Just h -> act h
    Nothing -> do
      hdbcTrace "Requested an ENV handle from disposed wrapper. Throwing."
      throwSqlError SqlError
        { seState = ""
        , seNativeError = -1
        , seErrorMsg = "Tried to use a disposed ODBC Environment handle"
        }

data DbcWrapper = DbcWrapper
  { connHandle :: RWVar (Maybe SQLHDBC) -- ^ If there is Nothing here, connection is already freed
  , connEnv    :: EnvWrapper
  }

-- This one implicitly allocates and initializes an environment.
sqlAllocDbc :: EnvWrapper -> IO DbcWrapper
sqlAllocDbc env = do
  hDbc <- withEnvOrDie env $ \hEnv -> alloca $ \(pdbcptr :: Ptr SQLHDBC) -> do
    retVal <- c_sqlAllocHandle sQL_HANDLE_DBC (castPtr hEnv) (castPtr pdbcptr)
    checkError "sqlAllocConn/SQLAllocHandle" (EnvHandle hEnv) retVal
    peek pdbcptr

  handleVar <- RWV.new $ Just hDbc
  let wrapper = DbcWrapper handleVar env

  addFinalizer wrapper $ freeDbcIfNotAlready False wrapper
  return wrapper


-- | Tries to perform disconnect and free resources used by connection. If SQLDisconnect call
-- fails, an exception gets thrown and connection resources aren't freed.
tryDisconnectAndFree :: DbcWrapper -> IO ()
tryDisconnectAndFree = freeDbcIfNotAlready True

freeDbcIfNotAlready :: Bool -> DbcWrapper -> IO ()
freeDbcIfNotAlready checkDisconnect dbc = do
  RWV.modify_ (connHandle dbc) $ \maybeHandle -> do
    F.forM_ maybeHandle $ \hDbc -> do
      hdbcTrace $ "Freeing connection with handle " ++ show hDbc
      disconnectResult <- c_sqlDisconnect hDbc
      when checkDisconnect $ checkError "freeDbcIfNotAlready/SQLDisconnect" (DbcHandle hDbc) disconnectResult
      void $ c_sqlFreeHandle sQL_HANDLE_DBC (castPtr hDbc)
    return Nothing
  freeEnvIfNotAlready $ connEnv dbc

withMaybeDbc :: DbcWrapper -> (Maybe SQLHDBC -> IO a) -> IO a
withMaybeDbc = RWV.with . connHandle

withDbcOrDie :: DbcWrapper -> (SQLHDBC -> IO a) -> IO a
withDbcOrDie dw act = withMaybeDbc dw $ \maybeHandle ->
  case maybeHandle of
    Just h -> act h
    Nothing -> do
      hdbcTrace "Requested a DBC handle from a disposed wrapper. Throwing"
      throwSqlError SqlError
        { seState = ""
        , seNativeError = -1
        , seErrorMsg = "Tried to use a disposed ODBC Connection handle"
        }

data StmtWrapper = StmtWrapper
  { stmtHandle :: MVar (Maybe SQLHSTMT) -- ^ If there is Nothing here, statement is already finished
  , stmtConn   :: DbcWrapper
  }

sqlAllocStmt :: DbcWrapper -> IO StmtWrapper
sqlAllocStmt dbc = do
  hStmt <- withDbcOrDie dbc $ \hDbc -> alloca $ \(psthptr :: Ptr SQLHSTMT) -> do
    retVal <- c_sqlAllocHandle sQL_HANDLE_STMT (castPtr hDbc) (castPtr psthptr)
    checkError "sqlAllocStmt/SQLAllocHandle" (DbcHandle hDbc) retVal
    peek psthptr

  handleVar <- newMVar $ Just hStmt
  let wrapper = StmtWrapper handleVar dbc

  addFinalizer wrapper $ freeStmtIfNotAlready wrapper
  return wrapper

freeStmtIfNotAlready :: StmtWrapper -> IO ()
freeStmtIfNotAlready stmt = modifyMVar_ (stmtHandle stmt) $ \maybeHandle -> do
  F.forM_ maybeHandle $ \hStmt -> do
    hdbcTrace $ "Freeing statement with handle " ++ show hStmt
    -- SQL Server might deadlock if closing handle to a statement in process of fetching
    -- network data so we have to cancel it explicitly. We also need to protect ourselves
    -- from trying to cancel or free a statement, which has its connection already finalized.
    RWV.with (connHandle $ stmtConn stmt) $ \maybeConn ->
      F.forM_ maybeConn $ \_ -> do
        void $ c_sqlCancel hStmt
        void $ c_sqlCloseCursor hStmt
        void $ c_sqlFreeHandle sQL_HANDLE_STMT (castPtr hStmt)
  return Nothing

withMaybeStmt :: StmtWrapper -> (Maybe SQLHSTMT -> IO a) -> IO a
withMaybeStmt = withMVar . stmtHandle

withStmtOrDie :: StmtWrapper -> (SQLHSTMT -> IO a) -> IO a
withStmtOrDie sw act = withMaybeStmt sw $ \maybeHandle ->
  case maybeHandle of
    Just h -> act h
    Nothing -> do
      hdbcTrace $ "Requested a STMT handle from a disposed wrapper. Throwing."
      throwSqlError SqlError
        { seState = ""
        , seNativeError = -1
        , seErrorMsg = "Tried to use a disposed ODBC Statement handle"
        }
