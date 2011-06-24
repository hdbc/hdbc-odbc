-- -*- mode: haskell; -*-
{-# CFILES hdbc-odbc-helper.c #-}
-- Above line for hugs
{-# LANGUAGE EmptyDataDecls #-}

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

import Foreign.C.String (castCUCharToChar)
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
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString.Unsafe as B
import Unsafe.Coerce (unsafeCoerce)

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
  { stomv      :: MVar (Maybe Stmt)
  , dbo        :: Conn
  , squery     :: String
  , colinfomv  :: MVar [(String, SqlColDesc)]
  , bindColsMV :: MVar [(BindCol, Ptr #{type SQLLEN})]
  }

-- FIXME: we currently do no prepare optimization whatsoever.

newSState :: Conn -> String -> IO SState
newSState indbo query =
    do newstomv     <- newMVar Nothing
       newcolinfomv <- newMVar []
       newBindCols  <- newMVar []
       return SState
         { stomv      = newstomv
         , dbo        = indbo
         , squery     = query
         , colinfomv  = newcolinfomv
         , bindColsMV = newBindCols
         }

wrapStmt :: SState -> Statement
wrapStmt sstate = Statement
  { execute        = fexecute sstate
  , executeRaw     = return ()
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

       bindArgs <- zipWithM (bindParam sthptr) args [1..]
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

getNumResultCols :: Ptr CStmt -> IO #{type SQLSMALLINT}
getNumResultCols sthptr = alloca $ \pcount ->
    do sqlNumResultCols sthptr pcount >>= checkError "SQLNumResultCols" 
                                          (StmtHandle sthptr)
       peek pcount

-- Bind a parameter column before execution.
bindParam :: Ptr CStmt -> SqlValue -> Word16
        -> IO (Maybe (Ptr #{type SQLLEN}, Ptr CChar))
bindParam sthptr arg icol =  alloca $ \pdtype ->
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
ffetchrow = ffetchrowGetData

ffetchrowGetData :: SState -> IO (Maybe [SqlValue])
ffetchrowGetData sstate = modifyMVar (stomv sstate) $ \stmt -> 
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
                               res <- mapM (getCol cstmt) [1..ncols]
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
                                 _ -> do bs <- B.packCStringLen (buf, fromIntegral len)
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
                        _ -> raiseError "sqlGetData" res (StmtHandle cstmt)

-- TODO: Check the return values of SQLFetch
-- TODO: Fix behaviour when strlen returns 0 (which indicates that SQLGetData
-- is needed)
ffetchrowBindCol :: SState -> IO (Maybe [SqlValue])
ffetchrowBindCol sstate = modifyMVar (stomv sstate) $ \stmt ->
  case stmt of
    Nothing -> do
      l "ffetchrowBindCol"
      return (stmt, Nothing)
    Just cmstmt -> withStmt cmstmt $ \cstmt -> do
      bindCols <- getBindCols sstate cstmt
      rc <- sqlFetch cstmt
      if rc == #{const SQL_NO_DATA}
        then do
          l "ffetchrowBindCol: no more rows"
          ffinish cmstmt
          return (Nothing, Nothing)
        else do
          l "ffetchrowBindcol: fetching data"
          checkError "sqlFetch" (StmtHandle cstmt) rc
          sqlValues <- mapM bindColToSqlValue bindCols
          return (stmt, Just sqlValues)

getBindCols :: SState -> Ptr CStmt -> IO [(BindCol, Ptr #{type SQLLEN})]
getBindCols sstate cstmt = do
  bindCols <- readMVar (bindColsMV sstate)
  case bindCols of
    [] -> do cols <- getNumResultCols cstmt
             pBindCols <- mapM (mkBindCol sstate cstmt) [1 .. cols]
             putMVar (bindColsMV sstate) pBindCols
             return pBindCols
    _  -> return bindCols

data ColBuf

data BindCol
  = BindColString  (Ptr CChar)
  | BindColWString (Ptr CWchar)
  | BindColUnknown (Ptr CChar)
  | BindColBit     (Ptr CUChar)
  | BindColTinyInt (Ptr CChar)
  | BindColShort   (Ptr CShort)
  | BindColLong    (Ptr CLong)
  | BindColBigInt  (Ptr CInt)    -- TODO: _int64
  | BindColFloat   (Ptr CFloat)
  | BindColDouble  (Ptr CDouble)
  | BindColBinary  (Ptr CUChar)
  | BindColDate -- TODO
-- struct tagDATE_STRUCT {
--    SQLSMALLINT year;
--    SQLUSMALLINT month;
--    SQLUSMALLINT day;  
-- } DATE_STRUCT;[a]
  | BindColTime -- TODO
-- struct tagTIME_STRUCT {
--    SQLUSMALLINT hour;
--    SQLUSMALLINT minute;
--    SQLUSMALLINT second;
-- } TIME_STRUCT;[a]
  | BindColTimestamp -- TODO
-- struct tagTIMESTAMP_STRUCT {
--    SQLSMALLINT year;
--    SQLUSMALLINT month;
--    SQLUSMALLINT day;
--    SQLUSMALLINT hour;
--    SQLUSMALLINT minute;
--    SQLUSMALLINT second;
--    SQLUINTEGER fraction;[b] 
-- } TIMESTAMP_STRUCT;[a]
  | BindColInterval -- TODO
-- typedef struct tagSQL_INTERVAL_STRUCT
-- {
--    SQLINTERVAL interval_type; 
--    SQLSMALLINT interval_sign;
--    union {
--          SQL_YEAR_MONTH_STRUCT   year_month;
--          SQL_DAY_SECOND_STRUCT   day_second;
--          } intval;
-- } SQL_INTERVAL_STRUCT;
-- typedef enum 
-- {
--    SQL_IS_YEAR = 1,
--    SQL_IS_MONTH = 2,
--    SQL_IS_DAY = 3,
--    SQL_IS_HOUR = 4,
--    SQL_IS_MINUTE = 5,
--    SQL_IS_SECOND = 6,
--    SQL_IS_YEAR_TO_MONTH = 7,
--    SQL_IS_DAY_TO_HOUR = 8,
--    SQL_IS_DAY_TO_MINUTE = 9,
--    SQL_IS_DAY_TO_SECOND = 10,
--    SQL_IS_HOUR_TO_MINUTE = 11,
--    SQL_IS_HOUR_TO_SECOND = 12,
--    SQL_IS_MINUTE_TO_SECOND = 13
-- } SQLINTERVAL;
-- 
-- typedef struct tagSQL_YEAR_MONTH
-- {
--    SQLUINTEGER year;
--    SQLUINTEGER month; 
-- } SQL_YEAR_MONTH_STRUCT;
-- 
-- typedef struct tagSQL_DAY_SECOND
-- {
--    SQLUINTEGER day;
--    SQLUINTEGER hour;
--    SQLUINTEGER minute;
--    SQLUINTEGER second;
--    SQLUINTEGER fraction;
-- } SQL_DAY_SECOND_STRUCT;
  | BindColGUID -- TODO
-- struct tagSQLGUID {
--    DWORD Data1;
--    WORD Data2;
--    WORD Data3;
--    BYTE Data4[8];
-- } SQLGUID;[k]

-- | This function binds the data in a column to a value of type
-- BindCol, using the default conversion scheme described here:
--     http://msdn.microsoft.com/en-us/library/ms716298(v=VS.85).aspx
-- The corresponding C types are here:
--     http://msdn.microsoft.com/en-us/library/ms714556(v=VS.85).aspx
-- These values are then ready for fetching.
-- Documentation about SQLBindCol can be found here:
--     http://msdn.microsoft.com/en-us/library/ms711010(v=vs.85).aspx
--
-- Our implementation follows this code:
--     http://publib.boulder.ibm.com/infocenter/iseries/v5r3/index.jsp?topic=%2Fcli%2Frzadpfndecol.htm
-- We have to make use of the column type and length information.
-- These are given by SQLDescribeCol, which is stored in colinfomv.
-- SQLDescribeCol can tell use the data type, and the size of a column (in
-- characters, so add 1 for the null terminator), or the number of decimal
-- digits that can be held.
-- To find out type, and how much memory to allocate, we could also use:
--    SQLColAttribute( ..., SQL_DESC_TYPE , ... )
--    SQLColAttribute( ..., SQL_DESC_OCTET_LENGTH , ... )
--
-- Further examples of how to use SQLBindCol are here, though these make use
-- of SQLDescribeCol:
--     http://msdn.microsoft.com/en-us/library/ms710118(v=vs.85).aspx
-- This implementation makes use of Column-Wise binding. Further improvements
-- might be had by using Row-Wise binding.
mkBindCol :: SState -> Ptr CStmt -> #{type SQLSMALLINT} -> IO (BindCol, Ptr #{type SQLLEN})
mkBindCol sstate cstmt col = do
  colInfo <- readMVar (colinfomv sstate)
  let colDesc = (snd (colInfo !! ((fromIntegral col) -1)))
  case colType colDesc of
    SqlCharT          -> mkBindColString    cstmt col' (colSize colDesc)
    SqlVarCharT       -> mkBindColString    cstmt col' (colSize colDesc)
    SqlLongVarCharT   -> mkBindColString    cstmt col' (colSize colDesc)
    SqlWCharT         -> mkBindColWString   cstmt col' (colSize colDesc)
    SqlWVarCharT      -> mkBindColWString   cstmt col' (colSize colDesc)
    SqlWLongVarCharT  -> mkBindColWString   cstmt col' (colSize colDesc)
    SqlDecimalT       -> mkBindColString    cstmt col' (colSize colDesc)
    SqlNumericT       -> mkBindColString    cstmt col' (colSize colDesc)
    SqlBitT           -> mkBindColBit       cstmt col' (colSize colDesc)
    SqlTinyIntT       -> mkBindColTinyInt   cstmt col' (colSize colDesc)
    SqlSmallIntT      -> mkBindColShort     cstmt col' (colSize colDesc)
    SqlIntegerT       -> mkBindColLong      cstmt col' (colSize colDesc)
    SqlBigIntT        -> mkBindColBigInt    cstmt col' (colSize colDesc)
    SqlRealT          -> mkBindColFloat     cstmt col' (colSize colDesc)
    SqlFloatT         -> mkBindColDouble    cstmt col' (colSize colDesc)
    SqlDoubleT        -> mkBindColDouble    cstmt col' (colSize colDesc)
    SqlBinaryT        -> mkBindColBinary    cstmt col' (colSize colDesc)
    SqlVarBinaryT     -> mkBindColBinary    cstmt col' (colSize colDesc)
    SqlLongVarBinaryT -> mkBindColBinary    cstmt col' (colSize colDesc)
    SqlDateT          -> mkBindColDate      cstmt col' (colSize colDesc)
    SqlTimeT          -> mkBindColTime      cstmt col' (colSize colDesc)
    SqlTimestampT     -> mkBindColTimestamp cstmt col' (colSize colDesc)
    SqlIntervalT i    -> mkBindColInterval  cstmt col' (colSize colDesc) i
    SqlGUIDT          -> mkBindColGUID      cstmt col' (colSize colDesc)
    SqlUnknownT s     -> mkBindColUnknown   cstmt col' (colSize colDesc) s
    _                 -> mkBindColUnknown   cstmt col' (colSize colDesc) "Unknown"
-- The following are not supported by ODBC:
--    SqlUTCDateTimeT
--    SqlUTCTimeT
--    SqlTimeWithZoneT
--    SqlTimestampWithZoneT
 where
  col' = fromIntegral col

-- The functions that follow do the marshalling from C into a Haskell type
mkBindColString cstmt col mColSize = do
  let colSize = fromMaybe 128 mColSize
  let bufLen  = sizeOf (undefined :: CChar) * colSize
  buf     <- mallocBytes bufLen
  pStrLen <- malloc
  sqlBindCol cstmt col #{const SQL_CHAR} (unsafeCoerce buf) (fromIntegral bufLen) pStrLen
  return (BindColString buf, pStrLen)

mkBindColWString cstmt col mColSize = do
  let colSize = fromMaybe 128 mColSize
  let bufLen  = sizeOf (undefined :: CWchar) * colSize
  buf     <- mallocBytes bufLen
  pStrLen <- malloc
  sqlBindCol cstmt col #{const SQL_CHAR} (unsafeCoerce buf) (fromIntegral bufLen) pStrLen
  return (BindColWString buf, pStrLen)

mkBindColBit cstmt col mColSize = undefined
mkBindColTinyInt cstmt col mColSize = undefined
mkBindColShort cstmt col mColSize = undefined
mkBindColLong cstmt col mColSize = undefined
mkBindColBigInt cstmt col mColSize = undefined
mkBindColFloat cstmt col mColSize = undefined
mkBindColDouble cstmt col mColSize = undefined
mkBindColBinary cstmt col mColSize = undefined
mkBindColDate cstmt col mColSize = undefined
mkBindColTime cstmt col mColSize = undefined
mkBindColTimestamp cstmt col mColSize = undefined
mkBindColInterval cstmt col mColSize = undefined
mkBindColGUID cstmt col mColSize = undefined

mkBindColUnknown cstmt col mColSize str = do
  let bufLen = 128
  buf     <- mallocBytes bufLen
  pStrLen <- malloc
  sqlBindCol cstmt col #{const SQL_CHAR} (unsafeCoerce buf) (fromIntegral bufLen) pStrLen
  return (BindColUnknown buf, pStrLen)

freeBindCol :: BindCol -> IO ()
freeBindCol (BindColString   buf) = free buf
freeBindCol (BindColString   buf) = free buf
freeBindCol (BindColWString  buf) = free buf
freeBindCol (BindColUnknown  buf) = free buf
freeBindCol (BindColBit      buf) = free buf
freeBindCol (BindColTinyInt  buf) = free buf
freeBindCol (BindColShort    buf) = free buf
freeBindCol (BindColLong     buf) = free buf
freeBindCol (BindColBigInt   buf) = free buf
freeBindCol (BindColFloat    buf) = free buf
freeBindCol (BindColDouble   buf) = free buf
freeBindCol (BindColBinary   buf) = free buf

-- Maybe it's best to separate BindCol and the Ptr StrLen so that
-- we can check for nulls straight away?
bindColToSqlValue :: (BindCol, Ptr #{type SQLLEN}) -> IO SqlValue
bindColToSqlValue (bindCol, pStrLen) = do
  strLen <- peek pStrLen
  case strLen of
    #{const SQL_NO_DATA}   -> undefined
    #{const SQL_NULL_DATA} -> return SqlNull
    _                      -> bindColToSqlValue' bindCol strLen

bindColToSqlValue' (BindColUnknown buf) strLen = do
  bs <- B.packCStringLen (buf, fromIntegral strLen)
  return $ SqlByteString bs

-- Each of these types must eventually correspond to one of the following
-- SqlValue members:
-- SqlChar Char
-- SqlString String	
-- SqlByteString ByteString	
-- SqlWord32 Word32	
-- SqlWord64 Word64	
-- SqlInt32 Int32	
-- SqlInt64 Int64	
-- SqlInteger Integer	
-- SqlBool Bool	
-- SqlDouble Double	
-- SqlRational Rational	
-- SqlLocalDate Day	Local YYYY-MM-DD (no timezone)
-- SqlLocalTimeOfDay TimeOfDay	Local HH:MM:SS (no timezone)
-- SqlZonedLocalTimeOfDay TimeOfDay TimeZone	Local HH:MM:SS -HHMM. Converts to and from (TimeOfDay, TimeZone).
-- SqlLocalTime LocalTime	Local YYYY-MM-DD HH:MM:SS (no timezone)
-- SqlZonedTime ZonedTime	Local YYYY-MM-DD HH:MM:SS -HHMM. Considered equal if both convert to the same UTC time.
-- SqlUTCTime UTCTime	UTC YYYY-MM-DD HH:MM:SS
-- SqlDiffTime NominalDiffTime	Calendar diff between seconds. Rendered as Integer when converted to String, but greater precision may be preserved for other types or to underlying database.
-- SqlPOSIXTime POSIXTime	Time as seconds since midnight Jan 1 1970 UTC. Integer rendering as for SqlDiffTime.
-- SqlEpochTime Integer	DEPRECATED Representation of ClockTime or CalendarTime. Use SqlPOSIXTime instead.
-- SqlTimeDiff Integer	DEPRECATED Representation of TimeDiff. Use SqlDiffTime instead.
-- SqlNull


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

-- TODO: Free the column memory
ffinish :: Stmt -> IO ()
ffinish stmt = withRawStmt stmt $ sqlFreeHandleSth_app 


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

foreign import #{CALLCONV} unsafe "sql.h SQLBindCol"
  sqlBindCol :: Ptr CStmt            -- ^ statement handle
             -> #{type SQLUSMALLINT} -- ^ Column number
             -> #{type SQLSMALLINT}  -- ^ target type
             -> Ptr ColBuf           -- ^ target value pointer (void * in C)
             -> #{type SQLLEN}       -- ^ buffer len
             -> Ptr (#{type SQLLEN}) -- ^ strlen_or_indptr
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
