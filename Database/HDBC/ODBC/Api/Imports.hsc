{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Database.HDBC.ODBC.Api.Imports
  ( sQL_HANDLE_ENV
  , sQL_HANDLE_DBC
  , sQL_HANDLE_STMT
  , sQL_CLOSE
  , sQL_UNBIND
  , sQL_RESET_PARAMS
  , c_sqlAllocHandle
  , c_sqlCancel
  , c_sqlCloseCursor
  , c_sqlDisconnect
  , c_sqlFreeHandle
  , c_sqlFreeStmt
  , c_sqlGetDiagRecW
  , c_sqlSetConnectAttr
  , c_sqlGetConnectAttr
  , sQL_ATTR_AUTOCOMMIT
  , sQL_AUTOCOMMIT_ON
  , sQL_AUTOCOMMIT_OFF
  , sQL_IS_UINTEGER
  ) where

import Database.HDBC.ODBC.Api.Types
import Database.HDBC.ODBC.Log
import Foreign.C.String
import Foreign.Ptr
import Text.Printf

#ifdef mingw32_HOST_OS
#include <windows.h>
#endif

#include <sql.h>
#include <sqlext.h>
#include <sqlucode.h>

#ifdef mingw32_HOST_OS
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

sQL_HANDLE_ENV :: SQLSMALLINT
sQL_HANDLE_ENV = #{const SQL_HANDLE_ENV}

sQL_HANDLE_STMT :: SQLSMALLINT
sQL_HANDLE_STMT = #{const SQL_HANDLE_STMT}

sQL_HANDLE_DBC :: SQLSMALLINT
sQL_HANDLE_DBC = #{const SQL_HANDLE_DBC}

foreign import #{CALLCONV} safe "sql.h SQLAllocHandle"
  imp_sqlAllocHandle :: SQLSMALLINT -> SQLHANDLE -> Ptr SQLHANDLE -> IO SQLRETURN

c_sqlAllocHandle :: SQLSMALLINT -> SQLHANDLE -> Ptr SQLHANDLE -> IO SQLRETURN
c_sqlAllocHandle handleType inputHandle outputHandlePtr = do
  result <- imp_sqlAllocHandle handleType inputHandle outputHandlePtr
  hdbcTrace $ printf "SQLAllocHandle(%d, %s, %s) returned %d" handleType (show inputHandle) (show outputHandlePtr) result
  return result

foreign import #{CALLCONV} safe "sql.h SQLFreeHandle"
  imp_sqlFreeHandle :: SQLSMALLINT -> SQLHANDLE -> IO SQLRETURN

c_sqlFreeHandle :: SQLSMALLINT -> SQLHANDLE -> IO SQLRETURN
c_sqlFreeHandle handleType handle = do
  result <- imp_sqlFreeHandle handleType handle
  hdbcTrace $ printf "SQLFreeHandle(%d, %s) returned %d" handleType (show handle) result
  return result

foreign import #{CALLCONV} safe "sql.h SQLCancel"
  imp_sqlCancel :: SQLHSTMT -> IO SQLRETURN

c_sqlCancel :: SQLHSTMT -> IO SQLRETURN
c_sqlCancel hStmt = do
  result <- imp_sqlCancel hStmt
  hdbcTrace $ printf "SQLCancel(%s) returned %d" (show hStmt) result
  return result

foreign import #{CALLCONV} safe "sql.h SQLCloseCursor"
  imp_sqlCloseCursor :: SQLHSTMT -> IO SQLRETURN

c_sqlCloseCursor :: SQLHSTMT -> IO SQLRETURN
c_sqlCloseCursor hStmt = do
  result <- imp_sqlCloseCursor hStmt
  hdbcTrace $ printf "SQLCloseCursor(%s) returned %d" (show hStmt) result
  return result

foreign import #{CALLCONV} safe "sql.h SQLDisconnect"
  imp_sqlDisconnect :: SQLHDBC -> IO SQLRETURN

c_sqlDisconnect :: SQLHDBC -> IO SQLRETURN
c_sqlDisconnect hDbc = do
  result <- imp_sqlDisconnect hDbc
  hdbcTrace $ printf "SQLDisconnect(%s) returned %d" (show hDbc) result
  return result

foreign import #{CALLCONV} safe "sql.h SQLGetDiagRecW"
  imp_sqlGetDiagRecW :: SQLSMALLINT -> Ptr () -> SQLSMALLINT -> CWString
                     -> Ptr SQLINTEGER -> CWString -> SQLSMALLINT
                     -> Ptr SQLSMALLINT -> IO SQLRETURN

c_sqlGetDiagRecW :: SQLSMALLINT -> Ptr () -> SQLSMALLINT -> CWString -> Ptr SQLINTEGER
                 -> CWString -> SQLSMALLINT -> Ptr SQLSMALLINT -> IO SQLRETURN
c_sqlGetDiagRecW handleType handle recNumber sqlState nativeErrorPtr messageText bufferLength textLengthPtr = do
  result <- imp_sqlGetDiagRecW handleType handle recNumber sqlState nativeErrorPtr messageText bufferLength textLengthPtr
  hdbcTrace $ printf "SqlGetDiagRec(%d, %s, %d, %s, %s, %s, %d, %s) returned %d"
                handleType (show handle) recNumber (show sqlState) (show nativeErrorPtr) (show messageText) bufferLength (show textLengthPtr) result
  return result

sQL_CLOSE :: SQLUSMALLINT
sQL_CLOSE = #{const SQL_CLOSE}

sQL_UNBIND :: SQLUSMALLINT
sQL_UNBIND = #{const SQL_UNBIND}

sQL_RESET_PARAMS :: SQLUSMALLINT
sQL_RESET_PARAMS = #{const SQL_RESET_PARAMS}

foreign import #{CALLCONV} safe "sql.h SQLFreeStmt"
  imp_sqlFreeStmt :: SQLHSTMT -> SQLUSMALLINT -> IO SQLRETURN

c_sqlFreeStmt :: SQLHSTMT -> SQLUSMALLINT -> IO SQLRETURN
c_sqlFreeStmt stmt option = do
  result <- imp_sqlFreeStmt stmt option
  hdbcTrace $ printf "SqlFreeStmt(%s, %d) returned %d" (show stmt) option result
  return result

foreign import #{CALLCONV} safe "sql.h SQLSetConnectAttr"
  imp_sqlSetConnectAttr :: SQLHDBC -> SQLINTEGER -> SQLPOINTER -> SQLINTEGER -> IO SQLRETURN

c_sqlSetConnectAttr :: SQLHDBC -> SQLINTEGER -> SQLPOINTER -> SQLINTEGER -> IO SQLRETURN
c_sqlSetConnectAttr conn attr valuePtr stringLength = do
  result <- imp_sqlSetConnectAttr conn attr valuePtr stringLength
  hdbcTrace $ printf "SQLSetConnectAttr (%s, %d, %s, %d) returned %d" (show conn) attr (show valuePtr) stringLength result
  return result

foreign import #{CALLCONV} safe "sql.h SQLGetConnectAttr"
  imp_sqlGetConnectAttr :: SQLHDBC -> SQLINTEGER -> SQLPOINTER -> SQLINTEGER -> Ptr SQLINTEGER -> IO SQLRETURN

c_sqlGetConnectAttr :: SQLHDBC -> SQLINTEGER -> SQLPOINTER -> SQLINTEGER -> Ptr SQLINTEGER -> IO SQLRETURN
c_sqlGetConnectAttr conn attr valuePtr bufferLength stringLengthPtr = do
  result <- imp_sqlGetConnectAttr conn attr valuePtr bufferLength stringLengthPtr
  hdbcTrace $ printf "SQLGetConnectAttr (%s, %d, %s, %d, %s) return %d"
    (show conn) attr (show valuePtr) bufferLength (show stringLengthPtr) result
  return result

sQL_ATTR_AUTOCOMMIT :: SQLINTEGER
sQL_ATTR_AUTOCOMMIT = #{const SQL_ATTR_AUTOCOMMIT}

sQL_AUTOCOMMIT_ON :: SQLUINTEGER
sQL_AUTOCOMMIT_ON = #{const SQL_AUTOCOMMIT_ON}

sQL_AUTOCOMMIT_OFF :: SQLUINTEGER
sQL_AUTOCOMMIT_OFF = #{const SQL_AUTOCOMMIT_OFF}

sQL_IS_UINTEGER :: SQLINTEGER
sQL_IS_UINTEGER = #{const SQL_IS_UINTEGER}
