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

#include <sql.h>
#include <sqlext.h>

{- | Connect to an ODBC server.

-}
connectODBC :: String -> IO Connection
connectODBC args = withCStringLen args $ \(cs, cslen) -> 
                   alloca $ \(penvptr::Ptr SqlHandle) ->
                   alloca $ \(pdbcptr::Ptr CConn) ->
         do -- Create the Environment Handle
            rc1 <- sqlAllocHandle #{const SQL_HANDLE_ENV}
                                  #{const SQL_NULL_HANDLE}
                                   penvptr
            envptr <- peek penvptr
            wrappedenvptr <- wrappenv envptr
            fenvptr <- newForeignPtr sqlFreeHandleEnv_ptr wrappedenvptr
            checkError "connectODBC/alloc env" fenvptr rc1
            sqlSetEnvAttr envptr #{const SQL_ATTR_ODBC_VERSION}
                              #{const SQL_OV_ODBC3} 0

            -- Create the DBC handle.
            sqlAllocHandle #{const SQL_HANDLE_DBC} envptr pdbcptr 
                          >>= checkError "connectODBC/alloc dbc" fenvptr
            dbcptr <- peek pdbcptr
            wrappeddbcptr <- wrapconn dbcptr
            fdbcptr <- newForeignPtr sqlFreeHandleDbc_ptr wrappeddbcptr

            -- Now connect.
            sqlDriverConnect dbcptr nullPtr cs cslen nullPtr 0 nullPtr
                             #{const SQL_DRIVER_COMPLETE}
                              >>= checkError "connectODBC/sqlDriverConnect" fenvptr
            mkConn args fenvptr fdbcptr

-- FIXME: environment vars may have changed, should use pgsql enquiries
-- for clone.
mkConn :: String -> Env -> Conn -> IO Connection
mkConn args ienv iconn = withConn iconn $
  \cconn -> 
    do begin_transaction conn
       let protover = "FIXME"
       let serverver = "FIXME"
       let clientver = "FIXME"
       let conninfo = ConnInfo {conn = iconn, env = ienv}
       return $ Connection {
                            disconnect = fdisconnect iconn,
                            commit = fcommit iconn,
                            rollback = frollback iconn,
                            run = frun iconn,
                            prepare = newSth iconn,
                            -- FIXME: add clone
                            hdbcDriverName = "odbc",
                            hdbcClientVer = clientver,
                            proxiedClientName = "FIXME",
                            proxiedClientVer = show protover,
                            dbServerVer = show serverver,
                            getTables = fgetTables iconn}

--------------------------------------------------
-- Guts here
--------------------------------------------------

begin_transaction :: ConnInfo -> IO ()
begin_transaction o = frun o "BEGIN" [] >> return ()

frun o query args =
    do sth <- newSth o query
       res <- execute sth args
       finish sth
       return res

fcommit o = do frun o "COMMIT" []
               begin_transaction o
frollback o =  do frun o "ROLLBACK" []
                  begin_transaction o

fdisconnect conninfo  = withRawConn (conn conninfo) $ \rawconn -> 
   sqlFreeHandleDbc_app rawconn >>= checkError "disconnect" (env conninfo)

foreign import ccall unsafe "sql.h SQLAllocHandle"
  sqlAllocHandle :: #{type SQLSMALLINT} -> SqlHandle -> 
                    Ptr SqlHandle -> IO (#{type SQLRETURN})

foreign import ccall unsafe "hdbc-odbc-helper.h wrapobj"
  wrapenv :: Ptr CEnv -> IO (Ptr WrappedCEnv)

foreign import ccall unsafe "hdbc-odbc-helper.h wrapobj"
  wrapconn :: Ptr CConn -> IO (Ptr WrappedCConn)

foreign import ccall unsafe "hdbc-odbc-helper.h sqlFreeHandleEnv_fptr"
  sqlFreeHandleEnv_ptr :: FunPtr (Ptr WrappedCEnv -> IO ())

foreign import ccall unsafe "hdbc-odbc-helper.h sqlFreeHandleDbc_fptr"
  sqlFreeHandleDbc_ptr :: FunPtr (Ptr WrappedCEnv -> IO ())

foreign import ccall unsafe "hdbc-odbc-helper.h sqlFreeHandleDbc_app"
  sqlFreeHandleDbc_app :: Ptr WrappedCEnv -> IO (#{type SQLRETURN})


  wrapconn :: Ptr CConn -> IO (Ptr WrappedCConn)


