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
  , c_sqlGetDiagRec
  ) where

import Database.HDBC.ODBC.Api.Types
import Database.HDBC.ODBC.Log
import Foreign.C.String
import Foreign.Ptr
import Formatting

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
  hdbcTraceLT $ format ("SQLAllocHandle(" % int % ", " % shown % ", " % shown % ") returned " % int) handleType inputHandle outputHandlePtr result
  return result

foreign import #{CALLCONV} safe "sql.h SQLFreeHandle"
  imp_sqlFreeHandle :: SQLSMALLINT -> SQLHANDLE -> IO SQLRETURN

c_sqlFreeHandle :: SQLSMALLINT -> SQLHANDLE -> IO SQLRETURN
c_sqlFreeHandle handleType handle = do
  result <- imp_sqlFreeHandle handleType handle
  hdbcTraceLT $ format ("SQLFreeHandle(" % int % ", " % shown % ") returned " % int) handleType handle result
  return result

foreign import #{CALLCONV} safe "sql.h SQLCancel"
  imp_sqlCancel :: SQLHSTMT -> IO SQLRETURN

c_sqlCancel :: SQLHSTMT -> IO SQLRETURN
c_sqlCancel hStmt = do
  result <- imp_sqlCancel hStmt
  hdbcTraceLT $ format ("SQLCancel(" % shown % ") returned " % int) hStmt result
  return result

foreign import #{CALLCONV} safe "sql.h SQLCloseCursor"
  imp_sqlCloseCursor :: SQLHSTMT -> IO SQLRETURN

c_sqlCloseCursor :: SQLHSTMT -> IO SQLRETURN
c_sqlCloseCursor hStmt = do
  result <- imp_sqlCloseCursor hStmt
  hdbcTraceLT $ format ("SQLCloseCursor(" % shown % ") returned " % int) hStmt result
  return result

foreign import #{CALLCONV} safe "sql.h SQLDisconnect"
  imp_sqlDisconnect :: SQLHDBC -> IO SQLRETURN

c_sqlDisconnect hDbc = do
  result <- imp_sqlDisconnect hDbc
  hdbcTraceLT $ format ("SQLDisconnect(" % shown % ") returned " % int) hDbc result
  return result

foreign import #{CALLCONV} safe "sql.h SQLGetDiagRec"
  imp_sqlGetDiagRec :: SQLSMALLINT -> Ptr () -> SQLSMALLINT -> CString
                    -> Ptr SQLINTEGER -> CString -> SQLSMALLINT
                    -> Ptr SQLSMALLINT -> IO SQLRETURN

c_sqlGetDiagRec :: SQLSMALLINT -> Ptr () -> SQLSMALLINT -> CString -> Ptr SQLINTEGER
                -> CString -> SQLSMALLINT -> Ptr SQLSMALLINT -> IO SQLRETURN
c_sqlGetDiagRec handleType handle recNumber sqlState nativeErrorPtr messageText bufferLength textLengthPtr = do
  result <- imp_sqlGetDiagRec handleType handle recNumber sqlState nativeErrorPtr messageText bufferLength textLengthPtr
  hdbcTraceLT $ format ("SqlGetDiagRec(" % int % ", " % shown % ", " % int % ", " % shown % ", " % shown %
                        ", " % shown % ", " % int % ", " % shown % ") returned " % int)
                handleType handle recNumber sqlState nativeErrorPtr messageText bufferLength textLengthPtr result
  return result

sQL_CLOSE :: SQLUSMALLINT
sQL_CLOSE = #{const SQL_CLOSE}

sQL_UNBIND :: SQLUSMALLINT
sQL_UNBIND = #{const SQL_UNBIND}

sQL_RESET_PARAMS :: SQLUSMALLINT
sQL_RESET_PARAMS = #{const SQL_RESET_PARAMS}

foreign import #{CALLCONV} safe "sql.h SQLFreeStmt"
  imp_sqlFreeStmt :: SQLHSTMT -> SQLUSMALLINT -> IO SQLRETURN

c_sqlFreeStmt stmt option = do
  result <- imp_sqlFreeStmt stmt option
  hdbcTraceLT $ format ("SqlFreeStmt(" % shown % ", " % int % ") returned " % int) stmt option result
  return result
