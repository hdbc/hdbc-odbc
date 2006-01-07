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
import Control.Exception
import System.IO

l _ = return ()
--l m = hPutStrLn stderr ("\n" ++ m)

#include <sql.h>
#include <sqlext.h>

data SState = 
    SState { stomv :: MVar (Maybe Stmt),
             nextrowmv :: MVar (CInt), -- -1 for no next row (empty); otherwise, next row to read.
             dbo :: ConnInfo,
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
fexecute sstate args = withConn (conn $ dbo sstate) $ \cconn ->
                       withCStringLen (squery sstate) $ \(cquery, cqlen) ->
                       mapM withCS args $ \cargs -> 
                       alloca $ \(psthptr::Ptr CStmt) ->
    do l "in fexecute"
       public_ffinish sstate    -- Sets nextrowmv to -1
       rc1 >= sqlAllocHandle #{const SQL_HANDLE_STMT} cconn psthptr
       sthptr <- peek psthptr
       wrappedsthptr <- wrappsth sthptr
       fsthptr <- newForeignPtr sqlFreeHandleSth_ptr wrappedsthptr
       checkError "execute allocHandle" (env sstate) rc1

       sqlPrepare sthptr cquery cqlen >>= 
            checkError "execute prepare" (env sstate)

       zipWithM_ (bindCol sthptr) cargs [1..]

       sqlExecute sthptr >>=
            checkError "execute execute" (env sstate) rc1
       rc <- getNumResultCols sthptr
       
       case rc of
         0 -> do sqlFreeHandleSth_app wrappedsthptr
                 rowcount <- getSqlRowCount sthptr
                 swapMVar (colnamemv sstate) []
                 touchForeignPtr fsthptr
                 return rowcount
         colcount -> do fgetcolnames sthptr >>= swapMVar (colnamemv sstate)
                        swapMVar (nextrowmv sstate) 0
                        swapMVar (stomv sstate) (Just fresptr)
                        touchForeignPtr fsthptr
                        return 0

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
                    if nextrow >= numrows
                       then do l "no more rows"
                               -- Don't use public_ffinish here
                               ffinish cmstmt
                               return (Nothing, ((-1), Nothing))
                       else do l "getting stuff"
                               ncols <- getNumResultCols cstmt
                               res <- mapM (getCol cstmt ) 
                                      [0..(ncols - 1)]
                               return (stmt, (nextrow + 1, Just res))
          getCol cstmt icol = 
             do size <- getColSize cstmt icol
                let bufsize = size + 127 -- Try to give extra space
                alloca $ \plen -> 
                 allocaBytes (bufsize + 1) $ \cs ->
                   do sqlGetData cstmt icol #{const SQL_CHAR} 
                                 cs bufsize plen
                      reslen <- peek plen
                      case reslen of
                        #{const SQL_NULL_DATA} -> return SqlNull
                        #{const SQL_NO_TOTAL} -> fail $ "Unexpected SQL_NO_TOTAL"
                        len -> do s <- peekCStringLen (cs, len)
                                  return (SqlString s)


fgetcolnames cstmt =
    do ncols <- getNumResultCols cstmt
       mapM getname [1..ncols]
    where getname icol = alloca $ \lp ->
                         allocaBytes 128 $ \cs ->
              do sqlDescribeCol cstmt icol cs 127 lp nullPtr nullPtr nullPtr nullPtr
                 peekCStringLen (cs, lp)

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


foreign import ccall unsafe "hdbc-postgresql-helper.h wrapobj"
  wrapstmt :: Ptr CStmt -> IO (Ptr WrappedCStmt)
