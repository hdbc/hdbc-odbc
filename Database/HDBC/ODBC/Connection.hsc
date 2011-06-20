-- -*- mode: haskell; -*-
{-# CFILES hdbc-odbc-helper.c #-}
-- Above line for hugs

module Database.HDBC.ODBC.Connection (connectODBC, Impl.Connection) where

import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.DriverUtils
import qualified Database.HDBC.ODBC.ConnectionImpl as Impl
import Database.HDBC.ODBC.Types
import Database.HDBC.ODBC.Statement
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Database.HDBC.ODBC.Utils
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import Data.Int
import Control.Concurrent.MVar
import Control.Monad (when)
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
connectODBC args = B.useAsCStringLen (BUTF8.fromString args) $ \(cs, cslen) -> 
                   alloca $ \(penvptr::Ptr (Ptr CEnv)) ->
                   alloca $ \(pdbcptr::Ptr (Ptr CConn)) ->
         do -- Create the Environment Handle
            rc1 <- sqlAllocHandle #{const SQL_HANDLE_ENV}
                                  nullPtr  -- {const SQL_NULL_HANDLE}
                                   (castPtr penvptr)
            envptr <- peek penvptr 

            checkError "connectODBC/alloc env" (EnvHandle envptr) rc1
            sqlSetEnvAttr envptr #{const SQL_ATTR_ODBC_VERSION}
                             (getSqlOvOdbc3) 0

            -- Create the DBC handle.
            sqlAllocHandle #{const SQL_HANDLE_DBC} (castPtr envptr) 
                               (castPtr pdbcptr)
                          >>= checkError "connectODBC/alloc dbc"
                                  (EnvHandle envptr)
            dbcptr <- peek pdbcptr
            wrappeddbcptr <- wrapconn dbcptr envptr nullPtr
            fdbcptr <- newForeignPtr sqlFreeHandleDbc_ptr wrappeddbcptr

            -- Now connect.
            sqlDriverConnect dbcptr nullPtr cs (fromIntegral cslen)
                             nullPtr 0 nullPtr
                             #{const SQL_DRIVER_NOPROMPT}
                              >>= checkError "connectODBC/sqlDriverConnect" 
                                  (DbcHandle dbcptr)
            mkConn args fdbcptr

-- FIXME: environment vars may have changed, should use pgsql enquiries
-- for clone.
mkConn :: String -> Conn -> IO Impl.Connection
mkConn args iconn = withConn iconn $ \cconn -> 
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

       when txnsupport
         (disableAutoCommit cconn
          >>= checkError "sqlSetConnectAttr" (DbcHandle cconn)
         )
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
                            Impl.describeTable = fdescribetable iconn
                           }

--------------------------------------------------
-- Guts here
--------------------------------------------------

frun conn children query args =
    do sth <- newSth conn children query
       res <- execute sth args
       finish sth
       return res

fcommit iconn = withConn iconn $ \cconn ->
    sqlEndTran #{const SQL_HANDLE_DBC} cconn #{const SQL_COMMIT}
    >>= checkError "sqlEndTran commit" (DbcHandle cconn)

frollback iconn = withConn iconn $ \cconn ->
    sqlEndTran #{const SQL_HANDLE_DBC} cconn #{const SQL_ROLLBACK}
    >>= checkError "sqlEndTran rollback" (DbcHandle cconn)

fdisconnect iconn mchildren  = withRawConn iconn $ \rawconn -> 
                               withConn iconn $ \llconn ->
    do closeAllChildren mchildren
       res <- sqlFreeHandleDbc_app rawconn
       -- FIXME: will this checkError segfault?
       checkError "disconnect" (DbcHandle $ llconn) res

foreign import #{CALLCONV} unsafe "sql.h SQLAllocHandle"
  sqlAllocHandle :: #{type SQLSMALLINT} -> Ptr () -> 
                    Ptr () -> IO (#{type SQLRETURN})

foreign import ccall unsafe "hdbc-odbc-helper.h wrapobjodbc_extra"
  wrapconn :: Ptr CConn -> Ptr CEnv -> Ptr WrappedCConn -> IO (Ptr WrappedCConn)

foreign import ccall unsafe "hdbc-odbc-helper.h &sqlFreeHandleDbc_finalizer"
  sqlFreeHandleDbc_ptr :: FunPtr (Ptr WrappedCConn -> IO ())

foreign import ccall unsafe "hdbc-odbc-helper.h sqlFreeHandleDbc_app"
  sqlFreeHandleDbc_app :: Ptr WrappedCConn -> IO (#{type SQLRETURN})

foreign import #{CALLCONV} unsafe "sql.h SQLSetEnvAttr"
  sqlSetEnvAttr :: Ptr CEnv -> #{type SQLINTEGER} -> 
                   Ptr () -> #{type SQLINTEGER} -> IO #{type SQLRETURN}

foreign import #{CALLCONV} unsafe "sql.h SQLDriverConnect"
  sqlDriverConnect :: Ptr CConn -> Ptr () -> CString -> #{type SQLSMALLINT}
                   -> CString -> #{type SQLSMALLINT}
                   -> Ptr #{type SQLSMALLINT} -> #{type SQLUSMALLINT}
                   -> IO #{type SQLRETURN}

foreign import ccall unsafe "hdbc-odbc-helper.h getSqlOvOdbc3"
  getSqlOvOdbc3 :: Ptr ()

foreign import ccall unsafe "hdbc-odbc-helper.h SQLSetConnectAttr"
  sqlSetConnectAttr :: Ptr CConn -> #{type SQLINTEGER} 
                    -> Ptr #{type SQLUINTEGER} -> #{type SQLINTEGER}
                    -> IO #{type SQLRETURN}

foreign import #{CALLCONV} unsafe "sql.h SQLEndTran"
  sqlEndTran :: #{type SQLSMALLINT} -> Ptr CConn -> #{type SQLSMALLINT}
             -> IO #{type SQLRETURN}

foreign import ccall unsafe "hdbc-odbc-helper.h disableAutoCommit"
  disableAutoCommit :: Ptr CConn -> IO #{type SQLRETURN}

foreign import #{CALLCONV} unsafe "sql.h SQLGetInfo"
  sqlGetInfo :: Ptr CConn -> #{type SQLUSMALLINT} -> Ptr () ->
                #{type SQLSMALLINT} -> Ptr #{type SQLSMALLINT} ->
                IO #{type SQLRETURN}
