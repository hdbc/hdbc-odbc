{- -*- mode: haskell; -*- 
-}

module Database.HDBC.ODBC.Utils where
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Int
import Database.HDBC(throwSqlError)
import Database.HDBC.Types
import Database.HDBC.ODBC.Types
import Foreign.C.Types
import Control.Exception
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8

#ifdef mingw32_HOST_OS
#include <windows.h>
#endif
#include "hdbc-odbc-helper.h"

#ifdef mingw32_HOST_OS
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

data SqlHandleT = EnvHandle (Ptr CEnv)
               | DbcHandle (Ptr CConn)
               | StmtHandle (Ptr CStmt)

checkError :: String -> SqlHandleT -> #{type SQLRETURN} -> IO ()
checkError msg o res =
        do let rc = sqlSucceeded res
           if rc == 0
               then raiseError msg res o
               else return ()

raiseError :: String -> #{type SQLRETURN} -> SqlHandleT -> IO a
raiseError msg code cconn =
    do info <- getdiag ht hp 1 
       throwSqlError $ SqlError {seState = show (map fst info),
                                 seNativeError = fromIntegral code,
                                 seErrorMsg = msg ++ ": " ++  
                                         show (map snd info)}
       where (ht, hp::(Ptr ())) = case cconn of
                          EnvHandle c -> (#{const SQL_HANDLE_ENV}, castPtr c)
                          DbcHandle c -> (#{const SQL_HANDLE_DBC}, castPtr c)
                          StmtHandle c -> (#{const SQL_HANDLE_STMT}, castPtr c)
             getdiag ht hp irow = allocaBytes 6 $ \csstate ->
                                  alloca $ \pnaterr ->
                                  allocaBytes 1025 $ \csmsg ->
                                  alloca $ \pmsglen ->
                 do ret <- sqlGetDiagRec ht hp irow csstate pnaterr
                           csmsg 1024 pmsglen
                    if sqlSucceeded ret == 0
                       then return []
                       else do state <- peekCStringLen (csstate, 5)
                               nat <- peek pnaterr
                               msglen <- peek pmsglen
                               msgbs <- B.packCStringLen (csmsg,
                                                          fromIntegral msglen)
                               let msg = BUTF8.toString msgbs
                               next <- getdiag ht hp (irow + 1)
                               return $ (state, 
                                         (show nat) ++ ": " ++ msg) : next

{- This is a little hairy.

We have a Conn object that is actually a finalizeonce wrapper around
the real object.  We use withConn to dereference the foreign pointer,
and then extract the pointer to the real object from the finalizeonce struct.

But, when we close the connection, we need the finalizeonce struct, so that's
done by withRawConn.

Ditto for statements. -}

withConn :: Conn -> (Ptr CConn -> IO b) -> IO b
withConn = genericUnwrap

withRawConn :: Conn -> (Ptr WrappedCConn -> IO b) -> IO b
withRawConn = withForeignPtr

withStmt :: Stmt -> (Ptr CStmt -> IO b) -> IO b
withStmt = genericUnwrap

withRawStmt :: Stmt -> (Ptr WrappedCStmt -> IO b) -> IO b
withRawStmt = withForeignPtr

withEnv :: Env -> (Ptr CEnv -> IO b) -> IO b
withEnv = genericUnwrap

withRawEnv :: Env -> (Ptr WrappedCEnv -> IO b) -> IO b
withRawEnv = withForeignPtr

withAnyArr0 :: (a -> IO (Ptr b)) -- ^ Function that transforms input data into pointer
            -> (Ptr b -> IO ())  -- ^ Function that frees generated data
            -> [a]               -- ^ List of input data
            -> (Ptr (Ptr b) -> IO c) -- ^ Action to run with the C array
            -> IO c             -- ^ Return value
withAnyArr0 input2ptract freeact inp action =
    bracket (mapM input2ptract inp)
            (\clist -> mapM_ freeact clist)
            (\clist -> withArray0 nullPtr clist action)


genericUnwrap :: ForeignPtr (Ptr a) -> (Ptr a -> IO b) -> IO b
genericUnwrap fptr action = withForeignPtr fptr (\structptr ->
    do objptr <- #{peek finalizeonce, encapobj} structptr
       action objptr
                                                )
isOK :: #{type SQLRETURN} -> Bool
isOK r = sqlSucceeded r /= 0

foreign import ccall safe "sqlSucceeded"
  sqlSucceeded :: #{type SQLRETURN} -> CInt

foreign import #{CALLCONV} safe "sql.h SQLGetDiagRec"
  sqlGetDiagRec :: #{type SQLSMALLINT} -> Ptr () -> 
                   #{type SQLSMALLINT} -> CString -> Ptr (#{type SQLINTEGER})
                   -> CString -> #{type SQLSMALLINT} 
                   -> Ptr (#{type SQLSMALLINT}) -> IO #{type SQLRETURN}
