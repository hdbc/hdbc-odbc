{-# LANGUAGE CPP, DoAndIfThenElse #-}
module Database.HDBC.ODBC.Api.Errors
  ( checkError
  , raiseError
  , sqlSucceeded
  ) where

import Control.Monad (unless)
import Database.HDBC (SqlError (..), throwSqlError)
import Database.HDBC.ODBC.Api.Imports
import Database.HDBC.ODBC.Api.Types
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

checkError :: String -> AnyHandle -> SQLRETURN -> IO ()
checkError msg o res =
  unless (sqlSucceeded res) $ raiseError msg res o

raiseError :: String -> SQLRETURN -> AnyHandle -> IO a
raiseError msg code cconn = do
    info <- getDiag ht hp 1
    throwSqlError SqlError
      { seState = show (map fst info)
      , seNativeError = fromIntegral code
      , seErrorMsg = msg ++ ": " ++ show (map snd info)
      }
  where
    (ht, hp) = case cconn of
                 EnvHandle c -> (sQL_HANDLE_ENV, castPtr c)
                 DbcHandle c -> (sQL_HANDLE_DBC, castPtr c)
                 StmtHandle c -> (sQL_HANDLE_STMT, castPtr c)

foreign import ccall safe "sqlSucceeded"
  c_sqlSucceeded :: SQLRETURN -> CInt

sqlSucceeded :: SQLRETURN -> Bool
sqlSucceeded x = c_sqlSucceeded x /= 0

getDiag :: SQLSMALLINT -> SQLHANDLE -> SQLSMALLINT -> IO [(String, String)]
getDiag ht hp irow =
  allocaBytes 6 $ \csstate ->
  alloca $ \pnaterr ->
  allocaBytes 1025 $ \csmsg ->
  alloca $ \pmsglen -> do
    ret <- c_sqlGetDiagRecW ht hp irow csstate pnaterr csmsg 1024 pmsglen
    if sqlSucceeded ret
     then do
      state <- peekCWStringLen (csstate, 5)
      nat <- peek pnaterr
      msglen <- peek pmsglen
      msgstr <- peekCWStringLen (csmsg, fromIntegral msglen)
      next <- getDiag ht hp (irow + 1)
      return $ (state, show nat ++ ": " ++ msgstr) : next
    else
      return []
