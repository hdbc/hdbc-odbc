{- -*- mode: haskell; -*- 
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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

module Database.HDBC.ODBC.Utils where
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Int
import Database.HDBC.Types
import Database.HDBC.ODBC.Types
import Foreign.C.Types
import Control.Exception
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Data.Word

#include "hdbc-odbc-helper.h"

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
       throwDyn $ SqlError {seState = show (map fst info),
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
                               msg <- peekCStringLen (csmsg, 
                                                      fromIntegral msglen)
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

withCStringArr0 :: [SqlValue] -> (Ptr CString -> IO a) -> IO a
withCStringArr0 inp action = withAnyArr0 convfunc freefunc inp action
    where convfunc SqlNull = return nullPtr
          convfunc x = newCString (fromSql x)
          freefunc x =
              if x == nullPtr
                 then return ()
                 else free x

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

foreign import ccall unsafe "sqlSucceeded"
  sqlSucceeded :: #{type SQLRETURN} -> CInt

foreign import ccall unsafe "sql.h SQLGetDiagRec"
  sqlGetDiagRec :: #{type SQLSMALLINT} -> Ptr () -> 
                   #{type SQLSMALLINT} -> CString -> Ptr (#{type SQLINTEGER})
                   -> CString -> #{type SQLSMALLINT} 
                   -> Ptr (#{type SQLSMALLINT}) -> IO #{type SQLRETURN}
