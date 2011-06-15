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
module Database.HDBC.ODBC.Statement (
   fGetQueryInfo,
   newSth,
   fgettables,
   fdescribetable
 ) where

import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.DriverUtils
import Database.HDBC.ODBC.Types
import Database.HDBC.ODBC.Utils
import Database.HDBC.ODBC.TypeConv

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Concurrent.MVar
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Monad
import Data.Word
import Data.Int
import Data.Maybe (catMaybes)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString.Unsafe as B

l :: String -> IO ()
l _ = return ()
--l m = hPutStrLn stderr ("\n" ++ m)

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

fGetQueryInfo :: Conn -> ChildList -> String
              -> IO ([SqlColDesc], [(String, SqlColDesc)])
fGetQueryInfo iconn children query =
    do l "in fGetQueryInfo"
       sstate <- newSState iconn query
       addChild children (wrapStmt sstate)   -- We get error if we forget this one. Not sure why.
       fakeExecute' sstate

fakeExecute' :: SState -> IO ([SqlColDesc], [(String, SqlColDesc)])
fakeExecute' sstate  = withConn (dbo sstate) $ \cconn ->
                       withCStringLen (squery sstate) $ \(cquery, cqlen) ->
                       alloca $ \(psthptr::Ptr (Ptr CStmt)) ->
    do l "in fexecute"
       -- public_ffinish sstate  
       rc1 <- sqlAllocStmtHandle #{const SQL_HANDLE_STMT} cconn psthptr
       sthptr <- peek psthptr
       wrappedsthptr <- withRawConn (dbo sstate)
                        (\rawconn -> wrapstmt sthptr rawconn)
       fsthptr <- newForeignPtr sqlFreeHandleSth_ptr wrappedsthptr
       checkError "execute allocHandle" (DbcHandle cconn) rc1

       sqlPrepare sthptr cquery (fromIntegral cqlen) >>= 
            checkError "execute prepare" (StmtHandle sthptr)

       -- parmCount <- getNumParams sthptr
       parmInfo <- fgetparminfo sthptr
       
       -- rc <- getNumResultCols sthptr
       colInfo <- fgetcolinfo sthptr
       return (parmInfo, colInfo)

-- | The Stament State
data SState = SState
  { stomv     :: MVar (Maybe Stmt)
  , dbo       :: Conn
  , squery    :: String
  , colinfomv :: MVar [(String, SqlColDesc)]
  }

-- FIXME: we currently do no prepare optimization whatsoever.

newSState :: Conn -> String -> IO SState
newSState indbo query =
    do newstomv <- newMVar Nothing
       newcolinfomv <- newMVar []
       return SState
         { stomv     = newstomv
         , dbo       = indbo
         , squery    = query
         , colinfomv = newcolinfomv
         }

wrapStmt :: SState -> Statement
wrapStmt sstate = Statement
  { execute        = fexecute sstate
  , executeMany    = fexecutemany sstate
  , finish         = public_ffinish sstate
  , fetchRow       = ffetchrow sstate
  , originalQuery  = (squery sstate)
  , getColumnNames = readMVar (colinfomv sstate) >>= (return . map fst)
  , describeResult = readMVar (colinfomv sstate)
  }

newSth :: Conn -> ChildList -> String -> IO Statement
newSth indbo mchildren query =
    do l "in newSth"
       sstate <- newSState indbo query
       let retval = wrapStmt sstate
       addChild mchildren retval
       return retval

makesth :: Conn -> [Char] -> IO (ForeignPtr WrappedCStmt)
makesth iconn name = alloca $ \(psthptr::Ptr (Ptr CStmt)) ->
                     withConn iconn $ \cconn -> 
                     withCString "" $ \emptycs ->
    do rc1 <- sqlAllocStmtHandle #{const SQL_HANDLE_STMT} cconn psthptr
       sthptr <- peek psthptr
       wrappedsthptr <- withRawConn iconn
                        (\rawconn -> wrapstmt sthptr rawconn)
       fsthptr <- newForeignPtr sqlFreeHandleSth_ptr wrappedsthptr
       checkError (name ++ " allocHandle") (DbcHandle cconn) rc1
       return fsthptr

wrapTheStmt :: Conn -> Stmt -> IO (Statement, SState)
wrapTheStmt iconn fsthptr =
    do sstate <- newSState iconn ""
       sstate <- newSState iconn ""
       swapMVar (stomv sstate) (Just fsthptr)
       let sth = wrapStmt sstate
       return (sth, sstate)

fgettables :: Conn -> IO [String]
fgettables iconn =
    do fsthptr <- makesth iconn "fgettables"
       l "fgettables: after makesth"
       withStmt fsthptr (\sthptr ->
                             simpleSqlTables sthptr >>=
                                checkError "gettables simpleSqlTables" 
                                               (StmtHandle sthptr)
                        )
       l "fgettables: after withStmt"
       (sth, sstate) <- wrapTheStmt iconn fsthptr
       withStmt fsthptr (\sthptr -> fgetcolinfo sthptr >>= swapMVar (colinfomv sstate))
       l "fgettables: after wrapTheStmt"
       results <- fetchAllRows' sth
       l ("fgettables: results: " ++ (show results))
       return $ map (\x -> fromSql (x !! 2)) results

fdescribetable :: Conn -> String -> IO [(String, SqlColDesc)]
fdescribetable iconn tablename = B.useAsCStringLen (BUTF8.fromString tablename) $ 
                                 \(cs, csl) ->
    do fsthptr <- makesth iconn "fdescribetable"
       withStmt fsthptr (\sthptr ->
                             simpleSqlColumns sthptr cs (fromIntegral csl) >>=
                               checkError "fdescribetable simpleSqlColumns"
                                          (StmtHandle sthptr)
                        )
       (sth, sstate) <- wrapTheStmt iconn fsthptr
       withStmt fsthptr (\sthptr -> fgetcolinfo sthptr >>= swapMVar (colinfomv sstate))
       results <- fetchAllRows' sth
       l (show results)
       return $ map fromOTypeCol results

{- For now, we try to just  handle things as simply as possible.
FIXME lots of room for improvement here (types, etc). -}
fexecute :: SState -> [SqlValue] -> IO Integer
fexecute sstate args =
  withConn (dbo sstate) $ \cconn ->
  B.useAsCStringLen (BUTF8.fromString (squery sstate)) $ \(cquery, cqlen) ->
  alloca $ \(psthptr::Ptr (Ptr CStmt)) ->
    do l $ "in fexecute: " ++ show (squery sstate) ++ show args
       public_ffinish sstate
       rc1 <- sqlAllocStmtHandle #{const SQL_HANDLE_STMT} cconn psthptr
       sthptr <- peek psthptr
       wrappedsthptr <- withRawConn (dbo sstate)
                        (\rawconn -> wrapstmt sthptr rawconn)
       fsthptr <- newForeignPtr sqlFreeHandleSth_ptr wrappedsthptr
       checkError "execute allocHandle" (DbcHandle cconn) rc1

       sqlPrepare sthptr cquery (fromIntegral cqlen) >>= 
            checkError "execute prepare" (StmtHandle sthptr)

       bindArgs <- zipWithM (bindCol sthptr) args [1..]
       l $ "Ready for sqlExecute: " ++ show (squery sstate) ++ show args
       r <- sqlExecute sthptr
       mapM_ (\(x, y) -> free x >> free y) (catMaybes bindArgs)

       case r of
         #{const SQL_NO_DATA} -> return () -- Update that did nothing
         x -> checkError "execute execute" (StmtHandle sthptr) x

       rc <- getNumResultCols sthptr

       case rc of
         0 -> do rowcount <- getSqlRowCount sthptr
                 ffinish fsthptr
                 swapMVar (colinfomv sstate) []
                 touchForeignPtr fsthptr
                 return (fromIntegral rowcount)
         colcount -> do fgetcolinfo sthptr >>= swapMVar (colinfomv sstate)
                        swapMVar (stomv sstate) (Just fsthptr)
                        touchForeignPtr fsthptr
                        return 0

getNumResultCols :: Ptr CStmt -> IO Int16
getNumResultCols sthptr = alloca $ \pcount ->
    do sqlNumResultCols sthptr pcount >>= checkError "SQLNumResultCols" 
                                          (StmtHandle sthptr)
       peek pcount

-- Bind a parameter column before execution.
bindCol :: Ptr CStmt -> SqlValue -> Word16
        -> IO (Maybe (Ptr #{type SQLLEN}, Ptr CChar))
bindCol sthptr arg icol =  alloca $ \pdtype ->
                           alloca $ \pcolsize ->
                           alloca $ \pdecdigits ->
                           alloca $ \pnullable ->
{- We have to start by getting the SQL type of the column so we can
   send the correct type back to the server.  Sigh.  If the ODBC
   backend won't tell us the type, we fake it.

   We've got an annoying situation with error handling.  Must make
   sure that all data is freed, but if there's an error, we have to raise
   it and the caller never gets to freed the allocated data to-date.
   So, make sure we either free of have foreignized everything before
   control passes out of this function. -}

    do l $ "Binding col " ++ show icol ++ ": " ++ show arg
       --TODO: seems like we ought to know the param type from inspecting the SqlValue
       rc1 <- sqlDescribeParam sthptr icol pdtype pcolsize pdecdigits pnullable
       l $ "rc1 is " ++ show (isOK rc1)
       when (not (isOK rc1)) $ -- Some drivers don't support that call
          do poke pdtype #{const SQL_CHAR}
             poke pcolsize 0
             poke pdecdigits 0
       coltype <- peek pdtype
       colsize <- peek pcolsize
       decdigits <- peek pdecdigits
       l $ "Results: " ++ show (coltype, colsize, decdigits)
       case arg of
         SqlNull -> -- NULL parameter, bind it as such.
                    do l "Binding null"
                       rc2 <- sqlBindParameter sthptr (fromIntegral icol)
                              #{const SQL_PARAM_INPUT}
                              #{const SQL_C_CHAR} coltype colsize decdigits
                              nullPtr 0 nullDataHDBC
                       checkError ("bindparameter NULL " ++ show icol)
                                      (StmtHandle sthptr) rc2
                       return Nothing
         x -> do -- Otherwise, we have to allocate RAM, make sure it's
                 -- not freed now, and pass it along...
                  (csptr, cslen) <- cstrUtf8BString (fromSql x)
                  do pcslen <- malloc 
                     poke pcslen (fromIntegral cslen)
                     rc2 <- sqlBindParameter sthptr (fromIntegral icol)
                       #{const SQL_PARAM_INPUT}
                       #{const SQL_C_CHAR} coltype 
                       (if isOK rc1 then colsize else fromIntegral cslen + 1) decdigits
                       csptr (fromIntegral cslen + 1) pcslen
                     if isOK rc2
                        then do -- We bound it.  Make foreignPtrs and return.
                                return $ Just (pcslen, csptr)
                        else do -- Binding failed.  Free the data and raise
                                -- error.
                                free pcslen
                                free csptr
                                checkError ("bindparameter " ++ show icol) 
                                               (StmtHandle sthptr) rc2
                                return Nothing -- will never get hit

getSqlRowCount :: Ptr CStmt -> IO Int32
getSqlRowCount cstmt = alloca $ \prows ->
     do sqlRowCount cstmt prows >>= checkError "SQLRowCount" (StmtHandle cstmt)
        peek prows
        --note: As of ODBC-3.52, the row count is only a C int, ie 32bit.


{- General algorithm: find out how many columns we have, check the type
of each to see if it's NULL.  If it's not, fetch it as text and return that.
-}

cstrUtf8BString :: B.ByteString -> IO CStringLen
cstrUtf8BString bs = do
    B.unsafeUseAsCStringLen bs $ \(s,len) -> do
        res <- mallocBytes (len+1)
        -- copy in
        copyBytes res s len
        -- null terminate
        poke (plusPtr res len) (0::CChar)
        -- return ptr
        return (res, len)


ffetchrow :: SState -> IO (Maybe [SqlValue])
ffetchrow sstate = modifyMVar (stomv sstate) $ \stmt -> 
             case stmt of
               Nothing -> l "ffr nos" >> return (stmt, Nothing)
               Just cmstmt -> withStmt cmstmt $ \cstmt ->
                 do rc <- sqlFetch cstmt
                    if rc == #{const SQL_NO_DATA}
                       then do l "no more rows"
                               -- Don't use public_ffinish here
                               ffinish cmstmt
                               return (Nothing, Nothing)
                       else do l "getting stuff"
                               checkError "sqlFetch" (StmtHandle cstmt) rc
                               ncols <- getNumResultCols cstmt
                               l $ "ncols: " ++ show ncols
                               res <- mapM (getCol cstmt ) 
                                      [1..ncols]
                               return (stmt, Just res)
    where getCol cstmt icol =
             do let defaultLen = 128
                colinfo <- readMVar (colinfomv sstate)
                l $ "getCol: colinfo is " ++ show colinfo ++ ", icol " ++ show icol
                let cBinding = case colType (snd (colinfo !! ((fromIntegral icol) - 1))) of
                                 SqlBinaryT -> #{const SQL_C_BINARY}
                                 SqlVarBinaryT -> #{const SQL_C_BINARY}
                                 SqlLongVarBinaryT -> #{const SQL_C_BINARY}
                                 _ -> #{const SQL_CHAR}
                alloca $ \plen ->
                 allocaBytes defaultLen $ \buf ->
                   do res <- sqlGetData cstmt (fromIntegral icol) cBinding
                                        buf (fromIntegral defaultLen) plen
                      case res of
                        #{const SQL_SUCCESS} ->
                            do len <- peek plen
                               case len of
                                 #{const SQL_NULL_DATA} -> return SqlNull
                                 #{const SQL_NO_TOTAL} -> fail $ "Unexpected SQL_NO_TOTAL"
                                 len -> do bs <- B.packCStringLen (buf, fromIntegral len)
                                           l $ "col is: " ++ show (BUTF8.toString bs)
                                           return (SqlByteString bs)
                        #{const SQL_SUCCESS_WITH_INFO} ->
                            do len <- peek plen
                               allocaBytes (fromIntegral len + 1) $ \buf2 ->
                                 do sqlGetData cstmt (fromIntegral icol) cBinding
                                               buf2 (fromIntegral len + 1) plen
                                               >>= checkError "sqlGetData" (StmtHandle cstmt)
                                    len2 <- peek plen
                                    let firstbuf = case cBinding of
                                                     #{const SQL_C_BINARY} -> defaultLen
                                                     _ -> defaultLen - 1 -- strip off NUL
                                    bs <- liftM2 (B.append) (B.packCStringLen (buf, firstbuf))
                                          (B.packCStringLen (buf2, fromIntegral len2))
                                    l $ "col is: " ++ (BUTF8.toString bs)
                                    return (SqlByteString bs)
                        res -> raiseError "sqlGetData" res (StmtHandle cstmt)

fgetcolinfo :: Ptr CStmt -> IO [(String, SqlColDesc)]
fgetcolinfo cstmt =
    do ncols <- getNumResultCols cstmt
       mapM getname [1..ncols]
    where getname icol = alloca $ \colnamelp ->
                         allocaBytes 128 $ \cscolname ->
                         alloca $ \datatypeptr ->
                         alloca $ \colsizeptr ->
                         alloca $ \nullableptr ->
              do sqlDescribeCol cstmt icol cscolname 127 colnamelp 
                                datatypeptr colsizeptr nullPtr nullableptr
                 colnamelen <- peek colnamelp
                 colnamebs <- B.packCStringLen (cscolname, fromIntegral colnamelen)
                 let colname = BUTF8.toString colnamebs
                 datatype <- peek datatypeptr
                 colsize <- peek colsizeptr
                 nullable <- peek nullableptr
                 return $ fromOTypeInfo colname datatype colsize nullable

-- FIXME: needs a faster algorithm.
fexecutemany :: SState -> [[SqlValue]] -> IO ()
fexecutemany sstate arglist =
    mapM_ (fexecute sstate) arglist >> return ()

-- Finish and change state
public_ffinish :: SState -> IO ()
public_ffinish sstate = 
    do l "public_ffinish"
       modifyMVar_ (stomv sstate) worker
    where worker Nothing = return Nothing
          worker (Just sth) = ffinish sth >> return Nothing

ffinish :: Stmt -> IO ()
ffinish p = withRawStmt p $ sqlFreeHandleSth_app 


foreign import ccall unsafe "hdbc-odbc-helper.h wrapobjodbc"
  wrapstmt :: Ptr CStmt -> Ptr WrappedCConn -> IO (Ptr WrappedCStmt)

foreign import #{CALLCONV} unsafe "sql.h SQLDescribeCol"
  sqlDescribeCol :: Ptr CStmt   
                 -> #{type SQLSMALLINT} -- ^ Column number
                 -> CString     -- ^ Column name
                 -> #{type SQLSMALLINT} -- ^ Buffer length
                 -> Ptr (#{type SQLSMALLINT}) -- ^ name length ptr
                 -> Ptr (#{type SQLSMALLINT}) -- ^ data type ptr
                 -> Ptr (#{type SQLULEN}) -- ^ column size ptr
                 -> Ptr (#{type SQLSMALLINT}) -- ^ decimal digits ptr
                 -> Ptr (#{type SQLSMALLINT}) -- ^ nullable ptr
                 -> IO #{type SQLRETURN}

foreign import #{CALLCONV} unsafe "sql.h SQLGetData"
  sqlGetData :: Ptr CStmt       -- ^ statement handle
             -> #{type SQLUSMALLINT} -- ^ Column number
             -> #{type SQLSMALLINT} -- ^ target type
             -> CString -- ^ target value pointer (void * in C)
             -> #{type SQLLEN} -- ^ buffer len
             -> Ptr (#{type SQLLEN})
             -> IO #{type SQLRETURN}

foreign import ccall unsafe "hdbc-odbc-helper.h sqlFreeHandleSth_app"
  sqlFreeHandleSth_app :: Ptr WrappedCStmt -> IO ()

foreign import ccall unsafe "hdbc-odbc-helper.h &sqlFreeHandleSth_finalizer"
  sqlFreeHandleSth_ptr :: FunPtr (Ptr WrappedCStmt -> IO ())

foreign import #{CALLCONV} unsafe "sql.h SQLPrepare"
  sqlPrepare :: Ptr CStmt -> CString -> #{type SQLINTEGER} 
             -> IO #{type SQLRETURN}

foreign import #{CALLCONV} unsafe "sql.h SQLExecute"
  sqlExecute :: Ptr CStmt -> IO #{type SQLRETURN}

foreign import #{CALLCONV} unsafe "sql.h SQLAllocHandle"
  sqlAllocStmtHandle :: #{type SQLSMALLINT} -> Ptr CConn ->
                        Ptr (Ptr CStmt) -> IO #{type SQLRETURN}

foreign import #{CALLCONV} unsafe "sql.h SQLNumResultCols"
  sqlNumResultCols :: Ptr CStmt -> Ptr #{type SQLSMALLINT} 
                   -> IO #{type SQLRETURN}

foreign import #{CALLCONV} unsafe "sql.h SQLRowCount"
  sqlRowCount :: Ptr CStmt -> Ptr #{type SQLINTEGER} -> IO #{type SQLRETURN}

foreign import #{CALLCONV} unsafe "sql.h SQLBindParameter"
  sqlBindParameter :: Ptr CStmt -- ^ Statement handle
                   -> #{type SQLUSMALLINT} -- ^ Parameter Number
                   -> #{type SQLSMALLINT} -- ^ Input or output
                   -> #{type SQLSMALLINT} -- ^ Value type
                   -> #{type SQLSMALLINT} -- ^ Parameter type
                   -> #{type SQLULEN} -- ^ column size
                   -> #{type SQLSMALLINT} -- ^ decimal digits
                   -> CString   -- ^ Parameter value pointer
                   -> #{type SQLLEN} -- ^ buffer length
                   -> Ptr #{type SQLLEN} -- ^ strlen_or_indptr
                   -> IO #{type SQLRETURN}

foreign import ccall unsafe "hdbc-odbc-helper.h &nullDataHDBC"
  nullDataHDBC :: Ptr #{type SQLLEN}

foreign import #{CALLCONV} unsafe "sql.h SQLDescribeParam"
  sqlDescribeParam :: Ptr CStmt 
                   -> #{type SQLUSMALLINT} -- ^ parameter number
                   -> Ptr #{type SQLSMALLINT} -- ^ data type ptr
                   -> Ptr #{type SQLULEN} -- ^ parameter size ptr
                   -> Ptr #{type SQLSMALLINT} -- ^ dec digits ptr
                   -> Ptr #{type SQLSMALLINT} -- ^ nullable ptr
                   -> IO #{type SQLRETURN}

foreign import #{CALLCONV} unsafe "sql.h SQLFetch"
  sqlFetch :: Ptr CStmt -> IO #{type SQLRETURN}

foreign import ccall unsafe "hdbc-odbc-helper.h simpleSqlTables"
  simpleSqlTables :: Ptr CStmt -> IO #{type SQLRETURN}

foreign import ccall unsafe "hdbc-odbc-helper.h simpleSqlColumns"
  simpleSqlColumns :: Ptr CStmt -> Ptr CChar -> 
                      #{type SQLSMALLINT} -> IO #{type SQLRETURN}

fgetparminfo :: Ptr CStmt -> IO [SqlColDesc]
fgetparminfo cstmt =
    do ncols <- getNumParams cstmt
       mapM getname [1..ncols]
    where getname icol = -- alloca $ \colnamelp ->
                         -- allocaBytes 128 $ \cscolname ->
                         alloca $ \datatypeptr ->
                         alloca $ \colsizeptr ->
                         alloca $ \nullableptr ->
              do poke datatypeptr 127 -- to test if sqlDescribeParam actually writes something to the area
                 res <- sqlDescribeParam cstmt (fromInteger $ toInteger icol) -- cscolname 127 colnamelp 
                                  datatypeptr colsizeptr nullPtr nullableptr
                 putStrLn $ show res
                 -- We need proper error handling here. Not all ODBC drivers supports SQLDescribeParam.
                 -- Not supporting SQLDescribeParam is quite allright according to the ODBC standard.
                 datatype <- peek datatypeptr
                 colsize  <- peek colsizeptr
                 nullable <- peek nullableptr
                 return $ snd $ fromOTypeInfo "" datatype colsize nullable

getNumParams :: Ptr CStmt -> IO Int16
getNumParams sthptr = alloca $ \pcount ->
    do sqlNumParams sthptr pcount >>= checkError "SQLNumResultCols" 
                                          (StmtHandle sthptr)
       peek pcount

foreign import #{CALLCONV} unsafe "sql.h SQLNumParams"
  sqlNumParams :: Ptr CStmt -> Ptr #{type SQLSMALLINT} 
               -> IO #{type SQLRETURN}
