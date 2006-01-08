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
module Database.HDBC.ODBC.Statement where
import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.ODBC.Types
import Database.HDBC.ODBC.Utils
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Concurrent.MVar
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Monad
import Data.List
import Data.Word
import Data.Int
import Control.Exception
import System.IO
import Data.Maybe

l _ = return ()
--l m = hPutStrLn stderr ("\n" ++ m)

#include <sql.h>
#include <sqlext.h>

data SState = 
    SState { stomv :: MVar (Maybe Stmt),
             nextrowmv :: MVar (CInt), -- -1 for no next row (empty); otherwise, next row to read.
             dbo :: Conn,
             squery :: String,
             colnamemv :: MVar [String]}

-- FIXME: we currently do no prepare optimization whatsoever.

newSth :: Conn -> String -> IO Statement               
newSth indbo query = 
    do l "in newSth"
       newstomv <- newMVar Nothing
       newnextrowmv <- newMVar (-1)
       newcolnamemv <- newMVar []
       let sstate = SState {stomv = newstomv, nextrowmv = newnextrowmv,
                            dbo = indbo, squery = query,
                            colnamemv = newcolnamemv}
       return $ Statement {execute = fexecute sstate,
                           executeMany = fexecutemany sstate,
                           finish = public_ffinish sstate,
                           fetchRow = ffetchrow sstate,
                           originalQuery = query,
                           getColumnNames = readMVar (colnamemv sstate)}

{- For now, we try to just  handle things as simply as possible.
FIXME lots of room for improvement here (types, etc). -}
fexecute sstate args = withConn (dbo sstate) $ \cconn ->
                       withCStringLen (squery sstate) $ \(cquery, cqlen) ->
                       alloca $ \(psthptr::Ptr (Ptr CStmt)) ->
    do l "in fexecute"
       public_ffinish sstate    -- Sets nextrowmv to -1
       rc1 <- sqlAllocStmtHandle #{const SQL_HANDLE_STMT} cconn psthptr
       sthptr <- peek psthptr
       wrappedsthptr <- wrapstmt sthptr
       fsthptr <- newForeignPtr sqlFreeHandleSth_ptr wrappedsthptr
       checkError "execute allocHandle" (DbcHandle cconn) rc1

       sqlPrepare sthptr cquery (fromIntegral cqlen) >>= 
            checkError "execute prepare" (StmtHandle sthptr)

       cargsraw <- zipWithM (bindCol sthptr) args [1..]
       let cargs = catMaybes cargsraw

       sqlExecute sthptr >>=
            checkError "execute execute" (StmtHandle sthptr)

       mapM (\(a, b) -> touchForeignPtr a >> touchForeignPtr b) cargs
       rc <- getNumResultCols sthptr
       
       case rc of
         0 -> do rowcount <- getSqlRowCount sthptr
                 sqlFreeHandleSth_app wrappedsthptr
                 swapMVar (colnamemv sstate) []
                 touchForeignPtr fsthptr
                 return (fromIntegral rowcount)
         colcount -> do fgetcolnames sthptr >>= swapMVar (colnamemv sstate)
                        swapMVar (nextrowmv sstate) 0
                        swapMVar (stomv sstate) (Just fsthptr)
                        touchForeignPtr fsthptr
                        return 0

getNumResultCols sthptr = alloca $ \pcount ->
    do sqlNumResultCols sthptr pcount >>= checkError "SQLNumResultCols" 
                                          (StmtHandle sthptr)
       peek pcount
    
bindCol sthptr arg icol =  alloca $ \pdtype ->
                           alloca $ \pcolsize ->
                           alloca $ \pdecdigits ->
    do rc1 <- sqlDescribeParam sthptr icol pdtype pcolsize pdecdigits
                      nullPtr
       when (sqlSucceeded rc1 == 0) $ -- Some drivers don't support that call
          do poke pdtype #{const SQL_CHAR}
             poke pcolsize 0
             poke pdecdigits 0
       coltype <- peek pdtype
       colsize <- peek pcolsize
       decdigits <- peek pdecdigits
       case arg of
         SqlNull -> do rc1 <- sqlBindParameter sthptr (fromIntegral icol)
                              #{const SQL_PARAM_INPUT}
                              #{const SQL_CHAR} coltype colsize decdigits
                              nullPtr 0 nullData
                       checkError ("bindparameter " ++ show icol)
                                  (StmtHandle sthptr) rc1
                       return Nothing
         x -> do (csptr, cslen) <- newCStringLen (fromSql x)
                 with (fromIntegral cslen) $ \pcslen -> 
                  do fcsptr <- newForeignPtr finalizerFree csptr
                     fcslenptr <- newForeignPtr finalizerFree pcslen
                     sqlBindParameter sthptr (fromIntegral icol)
                       #{const SQL_PARAM_INPUT}
                       #{const SQL_CHAR} coltype colsize decdigits
                       csptr (fromIntegral cslen + 1) pcslen
                       >>= checkError ("bindparameter " ++ show icol) 
                           (StmtHandle sthptr)
                     return (Just (fcsptr, fcslenptr))
       
getSqlRowCount cstmt = alloca $ \prows ->
     do sqlRowCount cstmt prows >>= checkError "SQLRowCount" (StmtHandle cstmt)
        peek prows

{- General algorithm: find out how many columns we have, check the type
of each to see if it's NULL.  If it's not, fetch it as text and return that.
-}

ffetchrow :: SState -> IO (Maybe [SqlValue])
ffetchrow sstate = modifyMVar (nextrowmv sstate) dofetchrow
    where dofetchrow (-1) = l "ffr -1" >> return ((-1), Nothing)
          dofetchrow nextrow = modifyMVar (stomv sstate) $ \stmt -> 
             case stmt of
               Nothing -> l "ffr nos" >> return (stmt, ((-1), Nothing))
               Just cmstmt -> withStmt cmstmt $ \cstmt ->
                 do l $ "ffetchrow: " ++ show nextrow
                    numrows <- getSqlRowCount cstmt
                    l $ "numrows: " ++ show numrows
                    if nextrow >= fromIntegral numrows
                       then do l "no more rows"
                               -- Don't use public_ffinish here
                               ffinish cmstmt
                               return (Nothing, ((-1), Nothing))
                       else do l "getting stuff"
                               ncols <- getNumResultCols cstmt
                               res <- mapM (getCol cstmt ) 
                                      [1..ncols]
                               return (stmt, (nextrow + 1, Just res))
          getCol cstmt icol = alloca $ \psize ->
             do sqlDescribeCol cstmt icol nullPtr 0 nullPtr nullPtr
                               psize nullPtr nullPtr
                               >>= checkError "sqlDescribeCol" 
                                       (StmtHandle cstmt)
                size <- peek psize
                let bufsize = size + 127 -- Try to give extra space
                alloca $ \plen -> 
                 allocaBytes (fromIntegral bufsize + 1) $ \cs ->
                   do sqlGetData cstmt (fromIntegral icol) #{const SQL_CHAR} 
                                 cs (fromIntegral bufsize) plen
                      reslen <- peek plen
                      case reslen of
                        #{const SQL_NULL_DATA} -> return SqlNull
                        #{const SQL_NO_TOTAL} -> fail $ "Unexpected SQL_NO_TOTAL"
                        len -> do s <- peekCStringLen (cs, fromIntegral len)
                                  return (SqlString s)


fgetcolnames cstmt =
    do ncols <- getNumResultCols cstmt
       mapM getname [1..ncols]
    where getname icol = alloca $ \lp ->
                         allocaBytes 128 $ \cs ->
              do sqlDescribeCol cstmt icol cs 127 lp nullPtr nullPtr nullPtr nullPtr
                 len <- peek lp
                 peekCStringLen (cs, fromIntegral len)

-- FIXME: needs a faster algorithm.
fexecutemany :: SState -> [[SqlValue]] -> IO ()
fexecutemany sstate arglist =
    mapM_ (fexecute sstate) arglist >> return ()

-- Finish and change state
public_ffinish sstate = 
    do l "public_ffinish"
       swapMVar (nextrowmv sstate) (-1)
       modifyMVar_ (stomv sstate) worker
    where worker Nothing = return Nothing
          worker (Just sth) = ffinish sth >> return Nothing

ffinish :: Stmt -> IO ()
ffinish p = withRawStmt p $ sqlFreeHandleSth_app 


foreign import ccall unsafe "hdbc-odbc-helper.h wrapobj"
  wrapstmt :: Ptr CStmt -> IO (Ptr WrappedCStmt)

foreign import ccall unsafe "sql.h SQLDescribeCol"
  sqlDescribeCol :: Ptr CStmt   
                 -> #{type SQLSMALLINT} -- ^ Column number
                 -> CString     -- ^ Column name
                 -> #{type SQLSMALLINT} -- ^ Buffer length
                 -> Ptr (#{type SQLSMALLINT}) -- ^ name length ptr
                 -> Ptr (#{type SQLSMALLINT}) -- ^ data type ptr
                 -> Ptr (#{type SQLUINTEGER}) -- ^ column size ptr
                 -> Ptr (#{type SQLSMALLINT}) -- ^ decimal digits ptr
                 -> Ptr (#{type SQLSMALLINT}) -- ^ nullable ptr
                 -> IO #{type SQLRETURN}

foreign import ccall unsafe "sql.h SQLGetData"
  sqlGetData :: Ptr CStmt       -- ^ statement handle
             -> #{type SQLUSMALLINT} -- ^ Column number
             -> #{type SQLSMALLINT} -- ^ target type
             -> CString -- ^ target value pointer (void * in C)
             -> #{type SQLINTEGER} -- ^ buffer len
             -> Ptr (#{type SQLINTEGER})
             -> IO #{type SQLRETURN}

foreign import ccall unsafe "hdbc-odbc-helper.h sqlFreeHandleSth_app"
  sqlFreeHandleSth_app :: Ptr WrappedCStmt -> IO ()

foreign import ccall unsafe "hdbc-odbc-helper.h &sqlFreeHandleSth_finalizer"
  sqlFreeHandleSth_ptr :: FunPtr (Ptr WrappedCStmt -> IO ())

foreign import ccall unsafe "sql.h SQLPrepare"
  sqlPrepare :: Ptr CStmt -> CString -> #{type SQLINTEGER} 
             -> IO #{type SQLRETURN}

foreign import ccall unsafe "sql.h SQLExecute"
  sqlExecute :: Ptr CStmt -> IO #{type SQLRETURN}

foreign import ccall unsafe "sql.h SQLAllocHandle"
  sqlAllocStmtHandle :: #{type SQLSMALLINT} -> Ptr CConn ->
                        Ptr (Ptr CStmt) -> IO #{type SQLRETURN}

foreign import ccall unsafe "sql.h SQLNumResultCols"
  sqlNumResultCols :: Ptr CStmt -> Ptr #{type SQLSMALLINT} 
                   -> IO #{type SQLRETURN}

foreign import ccall unsafe "sql.h SQLRowCount"
  sqlRowCount :: Ptr CStmt -> Ptr #{type SQLINTEGER} -> IO #{type SQLRETURN}

foreign import ccall unsafe "sql.h SQLBindParameter"
  sqlBindParameter :: Ptr CStmt -- ^ Statement handle
                   -> #{type SQLUSMALLINT} -- ^ Parameter Number
                   -> #{type SQLSMALLINT} -- ^ Input or output
                   -> #{type SQLSMALLINT} -- ^ Value type
                   -> #{type SQLSMALLINT} -- ^ Parameter type
                   -> #{type SQLUINTEGER} -- ^ column size
                   -> #{type SQLSMALLINT} -- ^ decimal digits
                   -> CString   -- ^ Parameter value pointer
                   -> #{type SQLINTEGER} -- ^ buffer length
                   -> Ptr #{type SQLINTEGER} -- ^ strlen_or_indptr
                   -> IO #{type SQLRETURN}

foreign import ccall unsafe "hdbc-odbc-helper.h &nullData"
  nullData :: Ptr #{type SQLINTEGER}

foreign import ccall unsafe "sql.h SQLDescribeParam"
  sqlDescribeParam :: Ptr CStmt 
                   -> #{type SQLUSMALLINT} -- ^ parameter number
                   -> Ptr #{type SQLSMALLINT} -- ^ data type ptr
                   -> Ptr #{type SQLUINTEGER} -- ^ parameter size ptr
                   -> Ptr #{type SQLSMALLINT} -- ^ dec digits ptr
                   -> Ptr #{type SQLSMALLINT} -- ^ nullable ptr
                   -> IO #{type SQLRETURN}