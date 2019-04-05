-- -*- mode: haskell; -*-
{-# CFILES hdbc-odbc-helper.c #-}
-- Above line for hugs

module Database.HDBC.ODBC.Connection (connectODBC, Impl.Connection) where

import Database.HDBC.Types
import Database.HDBC.DriverUtils
import qualified Database.HDBC.ODBC.ConnectionImpl as Impl
import Database.HDBC.ODBC.Api.Imports
import Database.HDBC.ODBC.Api.Errors
import Database.HDBC.ODBC.Api.Types
import Database.HDBC.ODBC.Statement
import Database.HDBC.ODBC.Wrappers
import Foreign.C.String
import Foreign.Marshal hiding (void)
import Foreign.Storable
import Foreign.Ptr
import Data.Word
import Data.Int
import Control.Concurrent.MVar
import Control.Monad (when, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8

#ifdef mingw32_HOST_OS
#include <windows.h>
#endif
#include <sql.h>
#include <sqlext.h>

#ifdef mingw32_HOST_OS
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

{- | Connect to an ODBC server.

For information on the meaning of the passed string, please see:

<http://msdn2.microsoft.com/en-us/library/ms715433(VS.85).aspx>

An example string is:

>"DSN=hdbctest1"

This, and all other functions that use ODBC directly or indirectly, can raise
SqlErrors just like other HDBC backends.  The seErrorMsg field is specified
as a String in HDBC.  ODBC specifies this data as a list of strings.
Therefore, this driver uses show on the data from ODBC.  For friendly display,
or handling of individual component messages in your code, you can use
read on the seErrorMsg field in a context that expects @[String]@.

Important note for MySQL users:

Unless you are going to use InnoDB tables, you are strongly encouraged to set

>Option = 262144

in your odbc.ini (for Unix users), or to disable transaction support in your
DSN setup for Windows users.

If you fail to do this, the MySQL ODBC driver will incorrectly state that it
supports transactions.  dbTransactionSupport will incorrectly return True.
commit and rollback will then silently fail.  This is certainly /NOT/ what you
want.  It is a bug (or misfeature) in the MySQL driver, not in HDBC.

You should ignore this advice if you are using InnoDB tables.

-}
connectODBC :: String -> IO Impl.Connection
connectODBC args =
  B.useAsCStringLen (BUTF8.fromString args) $ \(cs, cslen) -> do
  -- Create the Environment Handle
  env <- sqlAllocEnv
  withEnvOrDie env $ \hEnv ->
    void $ sqlSetEnvAttr hEnv #{const SQL_ATTR_ODBC_VERSION} (getSqlOvOdbc3) 0

  -- Create the DBC handle.
  dbc <- sqlAllocDbc env
  -- Now connect.
  withDbcOrDie dbc $ \hDbc ->
    sqlDriverConnect hDbc nullPtr cs (fromIntegral cslen)
                     nullPtr 0 nullPtr #{const SQL_DRIVER_NOPROMPT}
    >>= checkError "connectODBC/sqlDriverConnect" (DbcHandle hDbc)

  mkConn args dbc

-- FIXME: environment vars may have changed, should use pgsql enquiries
-- for clone.
mkConn :: String -> DbcWrapper -> IO Impl.Connection
mkConn args iconn = withDbcOrDie iconn $ \cconn ->
                    alloca $ \plen ->
                    alloca $ \psqlusmallint ->
                    allocaBytes 128 $ \pbuf ->
    do
       children <- newMVar []
       sqlGetInfo cconn #{const SQL_DBMS_VER} (castPtr pbuf) 127 plen
         >>= checkError "sqlGetInfo SQL_DBMS_VER" (DbcHandle cconn)
       len <- peek plen
       serverver <- peekCStringLen (pbuf, fromIntegral len)

       sqlGetInfo cconn #{const SQL_DRIVER_VER} (castPtr pbuf) 127 plen
         >>= checkError "sqlGetInfo SQL_DRIVER_VER" (DbcHandle cconn)
       len <- peek plen
       proxiedclientver <- peekCStringLen (pbuf, fromIntegral len)

       sqlGetInfo cconn #{const SQL_ODBC_VER} (castPtr pbuf) 127 plen
         >>= checkError "sqlGetInfo SQL_ODBC_VER" (DbcHandle cconn)
       len <- peek plen
       clientver <- peekCStringLen (pbuf, fromIntegral len)

       sqlGetInfo cconn #{const SQL_DBMS_NAME} (castPtr pbuf) 127 plen
         >>= checkError "sqlGetInfo SQL_DBMS_NAME" (DbcHandle cconn)
       len <- peek plen
       clientname <- peekCStringLen (pbuf, fromIntegral len)

       sqlGetInfo cconn #{const SQL_TXN_CAPABLE} (castPtr psqlusmallint)
                      0 nullPtr
         >>= checkError "sqlGetInfo SQL_TXN_CAPABLE" (DbcHandle cconn)
       txninfo <- ((peek psqlusmallint)::IO (#{type SQLUSMALLINT}))
       let txnsupport = txninfo /= #{const SQL_TC_NONE}

       when txnsupport . void $ fSetAutoCommit cconn False
       return $ Impl.Connection {
                            Impl.getQueryInfo = fGetQueryInfo iconn children,
                            Impl.disconnect = fdisconnect iconn children,
                            Impl.commit = fcommit iconn,
                            Impl.rollback = frollback iconn,
                            Impl.run = frun iconn children,
                            Impl.prepare = newSth iconn children,
                            Impl.clone = connectODBC args,
                            -- FIXME: add clone
                            Impl.hdbcDriverName = "odbc",
                            Impl.hdbcClientVer = clientver,
                            Impl.proxiedClientName = clientname,
                            Impl.proxiedClientVer = proxiedclientver,
                            Impl.dbServerVer = serverver,
                            Impl.dbTransactionSupport = txnsupport,
                            Impl.getTables = fgettables iconn,
                            Impl.describeTable = fdescribetable iconn,
                            Impl.setAutoCommit = \x -> withDbcOrDie iconn $ \conn -> fSetAutoCommit conn x
                           }

--------------------------------------------------
-- Guts here
--------------------------------------------------

frun :: DbcWrapper -> ChildList -> String -> [SqlValue] -> IO Integer
frun conn children query args =
    do sth <- newSth conn children query
       res <- execute sth args
       finish sth
       return res

fcommit :: DbcWrapper -> IO ()
fcommit iconn = withDbcOrDie iconn $ \cconn ->
    sqlEndTran #{const SQL_HANDLE_DBC} cconn #{const SQL_COMMIT}
    >>= checkError "sqlEndTran commit" (DbcHandle cconn)

frollback :: DbcWrapper -> IO ()
frollback iconn = withDbcOrDie iconn $ \cconn ->
    sqlEndTran #{const SQL_HANDLE_DBC} cconn #{const SQL_ROLLBACK}
    >>= checkError "sqlEndTran rollback" (DbcHandle cconn)

fdisconnect :: DbcWrapper -> ChildList -> IO ()
fdisconnect iconn mchildren  = do
  closeAllChildren mchildren
  freeDbcIfNotAlready True iconn

fGetAutoCommit :: SQLHDBC -> IO Bool
fGetAutoCommit hdbc = do
  value <- with (0 :: SQLUINTEGER) $ \acBuf -> do
    c_sqlGetConnectAttr hdbc sQL_ATTR_AUTOCOMMIT (castPtr acBuf) sQL_IS_UINTEGER nullPtr
      >>= checkError "sqlGetConnectAttr" (DbcHandle hdbc)
    peek acBuf
  return $ value /= sQL_AUTOCOMMIT_OFF

fSetAutoCommit :: SQLHDBC -> Bool -> IO Bool
fSetAutoCommit hdbc newValue = do
  oldValue <- fGetAutoCommit hdbc
  let newValueRaw = if newValue then sQL_AUTOCOMMIT_ON else sQL_AUTOCOMMIT_OFF
  c_sqlSetConnectAttr hdbc sQL_ATTR_AUTOCOMMIT (wordPtrToPtr $ fromIntegral newValueRaw) sQL_IS_UINTEGER
    >>= checkError "sqlSetConnectAttr" (DbcHandle hdbc)
  return oldValue

foreign import #{CALLCONV} safe "sql.h SQLSetEnvAttr"
  sqlSetEnvAttr :: SQLHENV -> #{type SQLINTEGER} ->
                   Ptr () -> #{type SQLINTEGER} -> IO #{type SQLRETURN}

foreign import #{CALLCONV} safe "sql.h SQLDriverConnect"
  sqlDriverConnect :: SQLHDBC -> Ptr () -> CString -> #{type SQLSMALLINT}
                   -> CString -> #{type SQLSMALLINT}
                   -> Ptr #{type SQLSMALLINT} -> #{type SQLUSMALLINT}
                   -> IO #{type SQLRETURN}

foreign import ccall safe "hdbc-odbc-helper.h getSqlOvOdbc3"
  getSqlOvOdbc3 :: Ptr ()

foreign import #{CALLCONV} safe "sql.h SQLEndTran"
  sqlEndTran :: #{type SQLSMALLINT} -> SQLHDBC -> #{type SQLSMALLINT}
             -> IO #{type SQLRETURN}

foreign import #{CALLCONV} safe "sql.h SQLGetInfo"
  sqlGetInfo :: SQLHDBC -> #{type SQLUSMALLINT} -> Ptr () ->
                #{type SQLSMALLINT} -> Ptr #{type SQLSMALLINT} ->
                IO #{type SQLRETURN}
