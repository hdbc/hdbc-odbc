-- -*- mode: haskell; -*-
{-# CFILES hdbc-odbc-helper.c #-}
-- Above line for hugs
{-
Copyright (C) 2005-2006 John Goerzen <jgoerzen@complete.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

module Database.HDBC.ODBC.Connection (connectODBC) where

import Database.HDBC.Types
import Database.HDBC
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

#include <sql.h>
#include <sqlext.h>

{- | Connect to an ODBC server.

-}
connectODBC :: String -> IO Connection
connectODBC args = withCStringLen args $ \(cs, cslen) -> 
                   alloca $ \(penvptr::Ptr (Ptr CEnv)) ->
                   alloca $ \(pdbcptr::Ptr (Ptr CConn)) ->
         do -- Create the Environment Handle
            rc1 <- sqlAllocHandle #{const SQL_HANDLE_ENV}
                                  nullPtr -- #{const SQL_NULL_HANDLE}
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
            wrappeddbcptr <- wrapconn dbcptr envptr
            fdbcptr <- newForeignPtr sqlFreeHandleDbc_ptr wrappeddbcptr

            -- Now connect.
            sqlDriverConnect dbcptr nullPtr cs (fromIntegral cslen)
                             nullPtr 0 nullPtr
                             #{const SQL_DRIVER_COMPLETE}
                              >>= checkError "connectODBC/sqlDriverConnect" 
                                  (DbcHandle dbcptr)
            mkConn args fdbcptr

-- FIXME: environment vars may have changed, should use pgsql enquiries
-- for clone.
mkConn :: String -> Conn -> IO Connection
mkConn args iconn = withConn iconn $ \cconn -> 
    do let protover = "FIXME"
       let serverver = "FIXME"
       let clientver = "FIXME"
       enableAutoCommit cconn
         >>= checkError "sqlSetConnectAttr" (DbcHandle cconn)
       return $ Connection {
                            disconnect = fdisconnect iconn,
                            commit = fcommit iconn,
                            rollback = frollback iconn,
                            run = frun iconn,
                            prepare = newSth iconn,
                            clone = connectODBC args,
                            -- FIXME: add clone
                            hdbcDriverName = "odbc",
                            hdbcClientVer = clientver,
                            proxiedClientName = "FIXME",
                            proxiedClientVer = show protover,
                            dbServerVer = show serverver,
                            getTables = fgettables iconn
                           }

--------------------------------------------------
-- Guts here
--------------------------------------------------

frun conn query args =
    do sth <- newSth conn query
       res <- execute sth args
       finish sth
       return res

fcommit iconn = withConn iconn $ \cconn ->
    sqlEndTran #{const SQL_HANDLE_DBC} cconn #{const SQL_COMMIT}
    >>= checkError "sqlEndTran commit" (DbcHandle cconn)

frollback iconn = withConn iconn $ \cconn ->
    sqlEndTran #{const SQL_HANDLE_DBC} cconn #{const SQL_ROLLBACK}
    >>= checkError "sqlEndTran rollback" (DbcHandle cconn)

fgettables iconn = fail "getTables not yet supported in ODBC"
    

fdisconnect iconn  = withRawConn iconn $ \rawconn -> 
                        withConn iconn $ \llconn ->
   sqlFreeHandleDbc_app rawconn >>= checkError "disconnect" (DbcHandle $ llconn)

foreign import ccall unsafe "sql.h SQLAllocHandle"
  sqlAllocHandle :: #{type SQLSMALLINT} -> Ptr () -> 
                    Ptr () -> IO (#{type SQLRETURN})

foreign import ccall unsafe "hdbc-odbc-helper.h wrapobj"
  wrapenv :: Ptr CEnv -> IO (Ptr WrappedCEnv)

foreign import ccall unsafe "hdbc-odbc-helper.h wrapobj_extra"
  wrapconn :: Ptr CConn -> Ptr CEnv -> IO (Ptr WrappedCConn)

foreign import ccall unsafe "hdbc-odbc-helper.h &sqlFreeHandleEnv_finalizer"
  sqlFreeHandleEnv_ptr :: FunPtr (Ptr WrappedCEnv -> IO ())

foreign import ccall unsafe "hdbc-odbc-helper.h &sqlFreeHandleDbc_finalizer"
  sqlFreeHandleDbc_ptr :: FunPtr (Ptr WrappedCConn -> IO ())

foreign import ccall unsafe "hdbc-odbc-helper.h sqlFreeHandleDbc_app"
  sqlFreeHandleDbc_app :: Ptr WrappedCConn -> IO (#{type SQLRETURN})

foreign import ccall unsafe "sql.h SQLSetEnvAttr"
  sqlSetEnvAttr :: Ptr CEnv -> #{type SQLINTEGER} -> 
                   Ptr () -> #{type SQLINTEGER} -> IO #{type SQLRETURN}

foreign import ccall unsafe "sql.h SQLDriverConnect"
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

foreign import ccall unsafe "sql.h SQLEndTran"
  sqlEndTran :: #{type SQLSMALLINT} -> Ptr CConn -> #{type SQLSMALLINT}
             -> IO #{type SQLRETURN}

foreign import ccall unsafe "sql.h enableAutoCommit"
  enableAutoCommit :: Ptr CConn -> IO #{type SQLRETURN}
