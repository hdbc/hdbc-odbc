{-# LANGUAGE EmptyDataDecls #-}
module Database.HDBC.ODBC.Api.Types
  ( SQLENV, SQLDBC, SQLSTMT
  , SQLHENV, SQLHDBC, SQLHSTMT, SQLHANDLE
  , AnyHandle (..)
  , SQLRETURN, SQLSMALLINT, SQLINTEGER
  , SQLUSMALLINT, SQLUINTEGER, SQLPOINTER
  ) where

import Data.Int (Int16, Int32)
import Data.Word (Word16, Word32)
import Foreign.Ptr

#ifdef mingw32_HOST_OS
#include <windows.h>
#endif
#include <sql.h>

data SQLENV
data SQLDBC
data SQLSTMT

type SQLHENV = Ptr SQLENV
type SQLHDBC = Ptr SQLDBC
type SQLHSTMT = Ptr SQLSTMT
type SQLHANDLE = Ptr ()

data AnyHandle = EnvHandle SQLHENV
               | DbcHandle SQLHDBC
               | StmtHandle SQLHSTMT

type SQLRETURN = #{type SQLRETURN}
type SQLSMALLINT = #{type SQLSMALLINT}
type SQLINTEGER = #{type SQLINTEGER}
type SQLUSMALLINT = #{type SQLUSMALLINT}
type SQLUINTEGER = #{type SQLUINTEGER}
type SQLPOINTER = Ptr ()
