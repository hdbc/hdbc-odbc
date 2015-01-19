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
import Database.HDBC.ODBC.Api.Errors
import Database.HDBC.ODBC.Api.Imports
import Database.HDBC.ODBC.Api.Types
import Database.HDBC.ODBC.Utils
import Database.HDBC.ODBC.Log
import Database.HDBC.ODBC.TypeConv
import Database.HDBC.ODBC.Wrappers

import Foreign.C.String (castCUCharToChar)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Applicative
import Control.Concurrent.MVar
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Monad
import Data.Word
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (TimeOfDay(TimeOfDay), LocalTime(LocalTime))
import Data.Int
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString.Unsafe as B
import Unsafe.Coerce (unsafeCoerce)

import System.IO (hPutStrLn, stderr)
import Debug.Trace

import qualified Data.Foldable as F

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

fGetQueryInfo :: DbcWrapper -> ChildList -> String
              -> IO ([SqlColDesc], [(String, SqlColDesc)])
fGetQueryInfo iconn children query =
    do hdbcTrace "in fGetQueryInfo"
       sstate <- newSState iconn query
       addChild children (wrapStmt sstate)   -- We get error if we forget this one. Not sure why.
       fakeExecute' sstate

fakeExecute' :: SState -> IO ([SqlColDesc], [(String, SqlColDesc)])
fakeExecute' sstate = do
  hdbcTrace "fakeExecute'"
  withStmtOrDie (sstmt sstate) $ \hStmt ->
    withCStringLen (squery sstate) $ \(cquery, cqlen) -> do
      hdbcTrace "fakeExecute' got stmt handle"
      sqlPrepare hStmt cquery (fromIntegral cqlen) >>=
        checkError "fakeExecute' prepare" (StmtHandle hStmt)

      -- parmCount <- getNumParams sthptr
      parmInfo <- fgetparminfo hStmt

      -- rc <- getNumResultCols sthptr
      colInfo <- fgetcolinfo hStmt
      return (parmInfo, colInfo)

-- | The Stament State
data SState = SState
  { sstmt        :: StmtWrapper
  , squery       :: String
  , stmtPrepared :: MVar Bool
  , colinfomv    :: MVar [(String, SqlColDesc)]
  , bindColsMV   :: MVar (Maybe [(BindCol, Ptr #{type SQLLEN})])
  }

-- FIXME: we currently do no prepare optimization whatsoever.

newSState :: DbcWrapper -> String -> IO SState
newSState indbo query = SState
  <$> sqlAllocStmt indbo
  <*> pure query
  <*> newMVar False
  <*> newMVar []
  <*> newMVar Nothing

wrapStmt :: SState -> Statement
wrapStmt sstate = Statement
  { execute        = fexecute sstate
  , executeRaw     = return ()
  , executeMany    = fexecutemany sstate
  , finish         = ffinish sstate
  , finalize       = ffinalize sstate
  , fetchRow       = ffetchrow sstate
  , originalQuery  = (squery sstate)
  , getColumnNames = readMVar (colinfomv sstate) >>= (return . map fst)
  , describeResult = readMVar (colinfomv sstate)
  }

newSth :: DbcWrapper -> ChildList -> String -> IO Statement
newSth indbo mchildren query =
    do hdbcTrace "in newSth"
       sstate <- newSState indbo query
       let retval = wrapStmt sstate
       addChild mchildren retval
       return retval

fgettables :: DbcWrapper -> IO [String]
fgettables iconn = do
  hdbcTrace "fgettables"
  sstate <- newSState iconn ""
  withStmtOrDie (sstmt sstate) $ \hStmt -> do
    hdbcTrace "fgettables got stmt handle"
    simpleSqlTables hStmt >>= checkError "gettables simpleSqlTables" (StmtHandle hStmt)
    fgetcolinfo hStmt >>= swapMVar (colinfomv sstate)
  results <- fetchAllRows' $ wrapStmt sstate
  return $ map (\x -> fromSql (x !! 2)) results

fdescribetable :: DbcWrapper -> String -> IO [(String, SqlColDesc)]
fdescribetable iconn tablename = do
  hdbcTrace "fdescribetable"
  B.useAsCStringLen (BUTF8.fromString tablename) $ \(cs, csl) -> do
    sstate <- newSState iconn tablename
    withStmtOrDie (sstmt sstate) $ \hStmt -> do
      hdbcTrace "fdescribetable got stmt handle"
      simpleSqlColumns hStmt cs (fromIntegral csl) >>= checkError "fdescribetable simpleSqlColumns" (StmtHandle hStmt)
      fgetcolinfo hStmt >>= swapMVar (colinfomv sstate)
    results <- fetchAllRows' $ wrapStmt sstate
    hdbcTrace $ show results
    return $ map fromOTypeCol results

{- For now, we try to just  handle things as simply as possible.
FIXME lots of room for improvement here (types, etc). -}
fexecute :: SState -> [SqlValue] -> IO Integer
fexecute sstate args = do
  hdbcTrace $ "fexecute: " ++ show (squery sstate) ++ show args
  (finish, result) <- withStmtOrDie (sstmt sstate) $ \hStmt -> do
    hdbcTrace "fexecute got stmt handle"
    -- Realloc the statement
    modifyMVar_ (stmtPrepared sstate) $ \prep -> do
      unless prep $
        B.useAsCStringLen (BUTF8.fromString (squery sstate)) $ \(cquery, cqlen) -> do
          sqlPrepare hStmt cquery (fromIntegral cqlen) >>= checkError "execute prepare" (StmtHandle hStmt)
      return True -- Statement is always prepared after this block completes.

    bindArgs <- zipWithM (bindParam hStmt) args [1..]
    hdbcTrace $ "Ready for sqlExecute: " ++ show (squery sstate) ++ show args
    r <- sqlExecute hStmt
    mapM_ (\(x, y) -> free x >> free y) (catMaybes bindArgs)

    case r of
      #{const SQL_NO_DATA} -> return () -- Update that did nothing
      x -> checkError "execute execute" (StmtHandle hStmt) x

    rc <- getNumResultCols hStmt

    case rc of
      0 -> do rowcount <- getSqlRowCount hStmt
              return (True, fromIntegral rowcount)
      colcount -> do fgetcolinfo hStmt >>= swapMVar (colinfomv sstate)
                     return (False, 0)
  when finish $ ffinish sstate
  return result

getNumResultCols :: SQLHSTMT -> IO #{type SQLSMALLINT}
getNumResultCols sthptr = alloca $ \pcount ->
    do sqlNumResultCols sthptr pcount >>= checkError "SQLNumResultCols"
                                          (StmtHandle sthptr)
       peek pcount

-- Bind a parameter column before execution.
bindParam :: SQLHSTMT -> SqlValue -> Word16
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

    do hdbcTrace $ "Binding col " ++ show icol ++ ": " ++ show arg
       rc1 <- sqlDescribeParam sthptr icol pdtype pcolsize pdecdigits pnullable
       hdbcTrace $ "rc1 is " ++ show (sqlSucceeded rc1)
       coltype <- if sqlSucceeded rc1 then Just <$> peek pdtype else return Nothing
       colsize <- if sqlSucceeded rc1 then Just <$> peek pcolsize else return Nothing
       decdigits <- if sqlSucceeded rc1 then Just <$> peek pdecdigits else return Nothing
       hdbcTrace $ "Results: " ++ show (coltype, colsize, decdigits)
       case arg of
         SqlNull -> -- NULL parameter, bind it as such.
                    do hdbcTrace "Binding null"
                       rc2 <- sqlBindParameter sthptr (fromIntegral icol)
                              #{const SQL_PARAM_INPUT}
                              #{const SQL_C_CHAR}
                              (fromMaybe #{const SQL_CHAR} coltype)
                              (fromMaybe 0 colsize)
                              (fromMaybe 0 decdigits)
                              nullPtr 0 nullDataHDBC
                       checkError ("bindparameter NULL " ++ show icol)
                                      (StmtHandle sthptr) rc2
                       return Nothing
         x -> do -- Otherwise, we have to allocate RAM, make sure it's
                 -- not freed now, and pass it along...
                  boundValue <- bindSqlValue x
                  do pcslen <- malloc
                     poke pcslen . fromIntegral $ bvBufferSize boundValue
                     rc2 <- sqlBindParameter sthptr (fromIntegral icol)
                       #{const SQL_PARAM_INPUT}
                       (bvValueType boundValue)
                       (fromMaybe (bvDefaultColumnType boundValue) coltype)
                       (fromMaybe (bvDefaultColumnSize boundValue) colsize)
                       (fromMaybe (bvDefaultDecDigits boundValue) decdigits)
                       (castPtr $ bvBuffer boundValue)
                       (bvBufferSize boundValue)
                       pcslen
                     if sqlSucceeded rc2
                        then do -- We bound it.  Make foreignPtrs and return.
                                return $ Just (pcslen, castPtr $ bvBuffer boundValue)
                        else do -- Binding failed.  Free the data and raise
                                -- error.
                                free pcslen
                                free (bvBuffer boundValue)
                                checkError ("bindparameter " ++ show icol)
                                               (StmtHandle sthptr) rc2
                                return Nothing -- will never get hit

getSqlRowCount :: SQLHSTMT -> IO Int32
getSqlRowCount cstmt = alloca $ \prows ->
     do sqlRowCount cstmt prows >>= checkError "SQLRowCount" (StmtHandle cstmt)
        peek prows
        --note: As of ODBC-3.52, the row count is only a C int, ie 32bit.

data BoundValue = BoundValue
  -- | Type of the value in the buffer
  { bvValueType         :: !(#{type SQLSMALLINT})
  -- | Type of the SQL value to use if ODBC driver doesn't report one
  , bvDefaultColumnType :: !(#{type SQLSMALLINT})
  , bvDefaultColumnSize :: !(#{type SQLULEN})
  , bvDefaultDecDigits  :: !(#{type SQLSMALLINT})
  , bvBuffer            :: !(Ptr ())
  , bvBufferSize        :: !(#{type SQLLEN})
  } deriving (Show)

-- | Marshals given SqlValue returning intended ValueType, default ColumnType,
-- and a CStringLen with a buffer containing bound value. Pointer in the CStringLen
-- structure must be freed by caller
bindSqlValue :: SqlValue -> IO BoundValue
bindSqlValue sqlValue = case sqlValue of
  SqlString s -> do
-- GHC for Windows strings are implemented with wchar_t symbols, which are 16-bit UCS-2 and ODBC
-- driver expects exactly that type. On Linux machines things get harder because wchar_t is 32-bit
-- (UTF-32) while ODBC WCHAR might be either 16 or 32 bit. So on Linux we convert our string to
-- UTF-8 and pass it to the driver telling it that we are passing SQL_C_CHAR data for a SQL_WCHAR
-- column and hoping that the driver will convert back from UTF-8 to appropriate representation.
#ifdef mingw32_HOST_OS
    (wstrPtr, wstrLen) <- newCWStringLen s
    let result = BoundValue
          { bvValueType         = #{const SQL_C_WCHAR}
          , bvDefaultColumnType = #{const SQL_WCHAR}
          , bvDefaultColumnSize = fromIntegral wstrLen
          , bvDefaultDecDigits  = 0
          , bvBuffer            = castPtr wstrPtr
          , bvBufferSize        = fromIntegral $ wstrLen * #{size wchar_t}
          }
    hdbcTrace $ "bind SqlString " ++ s ++ ": " ++ show result
    return $! result
#else
    let utf8ByteString = BUTF8.fromString s
    B.unsafeUseAsCStringLen utf8ByteString $ \(unsafeStrPtr, strLen) -> do
      safeStrPtr <- mallocBytes strLen
      copyBytes safeStrPtr unsafeStrPtr strLen
      let result = BoundValue
            { bvValueType         = #{const SQL_C_CHAR}
            , bvDefaultColumnType = #{const SQL_WCHAR}
            , bvDefaultColumnSize = fromIntegral strLen
            , bvDefaultDecDigits  = 0
            , bvBuffer            = castPtr safeStrPtr
            , bvBufferSize        = fromIntegral strLen
            }
      hdbcTrace $ "bind SqlString " ++ s ++ ": " ++ show result
      return result
#endif
  SqlByteString bs -> B.unsafeUseAsCStringLen bs $ \(s,len) -> do
    res <- mallocBytes len
    copyBytes res s len
    let result = BoundValue
          { bvValueType         = #{const SQL_C_BINARY}
          , bvDefaultColumnType = #{const SQL_BINARY}
          , bvDefaultColumnSize = fromIntegral len
          , bvDefaultDecDigits  = 0
          , bvBuffer            = castPtr res
          , bvBufferSize        = fromIntegral len
          }
    hdbcTrace $ "bind SqlByteString " ++ show bs ++ ": " ++ show result
    return $! result
  -- | This is rather hacky, I just replicate the behaviour of a previous version
  x -> do
    hdbcTrace $ "bind other " ++ show x
    bsResult <- bindSqlValue $ SqlByteString (fromSql x)
    let result = bsResult
          { bvValueType         = #{const SQL_C_CHAR}
          , bvDefaultColumnType = #{const SQL_CHAR}
          }
    hdbcTrace $ "bound other " ++ show x ++ ": " ++ show result
    return $! result

ffetchrow :: SState -> IO (Maybe [SqlValue])
ffetchrow sstate = do
  result <- withMaybeStmt (sstmt sstate) $ \maybeStmt ->
    case maybeStmt of
      Nothing -> do
        hdbcTrace "ffetchrow: no statement"
        return Nothing
      Just hStmt -> do
        hdbcTrace "ffetchrow"
        bindCols <- getBindCols sstate hStmt
        hdbcTrace "ffetchrow: fetching"
        rc <- sqlFetch hStmt
        if rc == #{const SQL_NO_DATA}
          then do
            hdbcTrace "ffetchrow: no more rows"
            return Nothing
          else do
            hdbcTrace "ffetchrow: fetching data"
            checkError "sqlFetch" (StmtHandle hStmt) rc
            sqlValues <- if rc == #{const SQL_SUCCESS} || rc == #{const SQL_SUCCESS_WITH_INFO}
              then mapM (bindColToSqlValue hStmt) bindCols
              else raiseError "sqlGetData" rc (StmtHandle hStmt)
            return $ Just sqlValues
  case result of
    Just x -> return $ Just x
    Nothing -> do
      ffinish sstate
      return Nothing

getBindCols :: SState -> SQLHSTMT -> IO [(BindCol, Ptr #{type SQLLEN})]
getBindCols sstate cstmt = do
  hdbcTrace "getBindCols"
  modifyMVar (bindColsMV sstate) $ \mBindCols ->
    case mBindCols of
      Nothing -> do
        cols <- getNumResultCols cstmt
        pBindCols <- mapM (mkBindCol sstate cstmt) [1 .. cols]
        return (Just pBindCols, pBindCols)
      Just bindCols -> do
        return (mBindCols, bindCols)

-- This is only for String data. For binary fix should be very easy. Just check the column type and use buflen instead of buflen - 1
getLongColData cstmt bindCol = do
   let (BindColString buf bufLen col) = bindCol
   hdbcTrace $ "buflen: " ++ show bufLen
   bs <- B.packCStringLen (buf, fromIntegral (bufLen - 1))
   hdbcTrace $ "sql_no_total col " ++ show (BUTF8.toString bs)
   bs2 <- getRestLongColData cstmt #{const SQL_CHAR} col bs
   return $ SqlByteString bs2


getRestLongColData cstmt cBinding icol acc = do
  hdbcTrace "getLongColData"
  alloca $ \plen ->
   allocaBytes colBufSizeMaximum $ \buf ->
     do res <- sqlGetData cstmt (fromIntegral icol) cBinding
                          buf (fromIntegral colBufSizeMaximum) plen
        if res == #{const SQL_SUCCESS} || res == #{const SQL_SUCCESS_WITH_INFO}
           then do
                len <- peek plen
                if len == #{const SQL_NO_DATA}
                   then return acc
                   else do
                        let bufmax = fromIntegral $ colBufSizeMaximum - 1
                        bs <- B.packCStringLen (buf, fromIntegral (if len == #{const SQL_NO_TOTAL} || len > bufmax then bufmax else len))
                        hdbcTrace $ "sql_no_total col is: " ++ show (BUTF8.toString bs)
                        let newacc = B.append acc bs
                        if len /= #{const SQL_NO_TOTAL} && len <= bufmax
                           then return newacc
                           else getRestLongColData cstmt cBinding icol newacc
           else  raiseError "sqlGetData" res (StmtHandle cstmt)

-- TODO: This code does not deal well with data that is extremely large,
-- where multiple fetches are required.
getColData cstmt cBinding icol = do
  alloca $ \plen ->
   allocaBytes colBufSizeDefault $ \buf ->
     do res <- sqlGetData cstmt (fromIntegral icol) cBinding
                          buf (fromIntegral colBufSizeDefault) plen
        case res of
          #{const SQL_SUCCESS} ->
              do len <- peek plen
                 case len of
                   #{const SQL_NULL_DATA} -> return SqlNull
                   #{const SQL_NO_TOTAL} -> fail $ "Unexpected SQL_NO_TOTAL"
                   _ -> do bs <- B.packCStringLen (buf, fromIntegral len)
                           hdbcTrace $ "col is: " ++ show (BUTF8.toString bs)
                           return (SqlByteString bs)
          #{const SQL_SUCCESS_WITH_INFO} ->
              do len <- peek plen
                 allocaBytes (fromIntegral len + 1) $ \buf2 ->
                   do sqlGetData cstmt (fromIntegral icol) cBinding
                                 buf2 (fromIntegral len + 1) plen
                                 >>= checkError "sqlGetData" (StmtHandle cstmt)
                      len2 <- peek plen
                      let firstbuf = case cBinding of
                                       #{const SQL_C_BINARY} -> colBufSizeDefault
                                       _ -> colBufSizeDefault - 1 -- strip off NUL
                      bs <- liftM2 (B.append) (B.packCStringLen (buf, firstbuf))
                            (B.packCStringLen (buf2, fromIntegral len2))
                      hdbcTrace $ "col is: " ++ (BUTF8.toString bs)
                      return (SqlByteString bs)
          _ -> raiseError "sqlGetData" res (StmtHandle cstmt)

-- | ffetchrowBaseline is used for benchmarking fetches without the
-- overhead of marshalling values.
ffetchrowBaseline sstate = do
  hdbcTrace "ffetchrowBaseline"
  result <- withStmtOrDie (sstmt sstate) $ \hStmt -> do
    hdbcTrace "ffetchrowBaseline got stmt handle"
    rc <- sqlFetch hStmt
    if rc == #{const SQL_NO_DATA}
      then return Nothing
      else return (Just [])
  case result of
    Just x -> return $ Just x
    Nothing -> do
      ffinish sstate
      return Nothing

data ColBuf

-- These correspond to the C type identifiers found here:
--     http://msdn.microsoft.com/en-us/library/ms714556(v=VS.85).aspx
-- The Ptr values point to the appropriate C types
data BindCol
  = BindColString  (Ptr CChar) #{type SQLLEN} #{type SQLUSMALLINT}
  | BindColWString (Ptr CWchar) #{type SQLLEN} #{type SQLUSMALLINT}
  | BindColBit     (Ptr CUChar)
  | BindColTinyInt (Ptr CChar)
  | BindColShort   (Ptr CShort)
  | BindColLong    (Ptr CLong)
  | BindColBigInt  (Ptr #{type SQLBIGINT})
  | BindColFloat   (Ptr CFloat)
  | BindColDouble  (Ptr CDouble)
  | BindColBinary  (Ptr CUChar) #{type SQLLEN} #{type SQLUSMALLINT}
  | BindColDate    (Ptr StructDate)
  | BindColTime    (Ptr StructTime)
  | BindColTimestamp (Ptr StructTimestamp)
  | BindColGetData #{type SQLUSMALLINT}


-- Intervals and GUIDs have not been implemented, since there is no
-- equivalent SqlValue for these.
--
--  | BindColInterval
--      typedef struct tagSQL_INTERVAL_STRUCT
--      {
--         SQLINTERVAL interval_type;
--         SQLSMALLINT interval_sign;
--         union {
--               SQL_YEAR_MONTH_STRUCT   year_month;
--               SQL_DAY_SECOND_STRUCT   day_second;
--               } intval;
--      } SQL_INTERVAL_STRUCT;
--      typedef enum
--      {
--         SQL_IS_YEAR = 1,
--         SQL_IS_MONTH = 2,
--         SQL_IS_DAY = 3,
--         SQL_IS_HOUR = 4,
--         SQL_IS_MINUTE = 5,
--         SQL_IS_SECOND = 6,
--         SQL_IS_YEAR_TO_MONTH = 7,
--         SQL_IS_DAY_TO_HOUR = 8,
--         SQL_IS_DAY_TO_MINUTE = 9,
--         SQL_IS_DAY_TO_SECOND = 10,
--         SQL_IS_HOUR_TO_MINUTE = 11,
--         SQL_IS_HOUR_TO_SECOND = 12,
--         SQL_IS_MINUTE_TO_SECOND = 13
--      } SQLINTERVAL;
--
--      typedef struct tagSQL_YEAR_MONTH
--      {
--         SQLUINTEGER year;
--         SQLUINTEGER month;
--      } SQL_YEAR_MONTH_STRUCT;
--
--      typedef struct tagSQL_DAY_SECOND
--      {
--         SQLUINTEGER day;
--         SQLUINTEGER hour;
--         SQLUINTEGER minute;
--         SQLUINTEGER second;
--         SQLUINTEGER fraction;
--      } SQL_DAY_SECOND_STRUCT;
-- | BindColGUID (Ptr StructGUID)


-- | StructDate is used to marshal the DATE_STRUCT
-- This struct, and the ones which follow, are described here:
--     http://msdn.microsoft.com/en-us/library/ms714556(v=VS.85).aspx
data StructDate = StructDate
  #{type SQLSMALLINT}   -- year
  #{type SQLUSMALLINT}  -- month
  #{type SQLUSMALLINT}  -- day
 deriving Show

instance Storable StructDate where
  sizeOf _    = #{size DATE_STRUCT}
  alignment _ = alignment (undefined :: CLong)
  poke p (StructDate year month day) = do
    #{poke DATE_STRUCT, year}  p year
    #{poke DATE_STRUCT, month} p month
    #{poke DATE_STRUCT, day}   p day
  peek p = return StructDate
    `ap` (#{peek DATE_STRUCT, year}  p)
    `ap` (#{peek DATE_STRUCT, month} p)
    `ap` (#{peek DATE_STRUCT, day}   p)


-- | StructTime is used to marshals the TIME_STRUCT:
data StructTime = StructTime
  #{type SQLUSMALLINT} -- hour
  #{type SQLUSMALLINT} -- minute
  #{type SQLUSMALLINT} -- second

instance Storable StructTime where
  sizeOf _    = #{size TIME_STRUCT}
  alignment _ = alignment (undefined :: CLong)
  poke p (StructTime hour minute second) = do
    #{poke TIME_STRUCT, hour}   p hour
    #{poke TIME_STRUCT, minute} p minute
    #{poke TIME_STRUCT, second} p second
  peek p = return StructTime
    `ap` (#{peek TIME_STRUCT, hour}  p)
    `ap` (#{peek TIME_STRUCT, minute} p)
    `ap` (#{peek TIME_STRUCT, second}   p)

-- | StructTimestamp is used to marshal the TIMESTAMP_STRUCT;
data StructTimestamp = StructTimestamp
  #{type SQLSMALLINT}   -- year
  #{type SQLUSMALLINT}  -- month
  #{type SQLUSMALLINT}  -- day
  #{type SQLUSMALLINT}  -- hour
  #{type SQLUSMALLINT}  -- minute
  #{type SQLUSMALLINT}  -- second
  #{type SQLUINTEGER}   -- fraction

instance Storable StructTimestamp where
  sizeOf _    = #{size TIMESTAMP_STRUCT}
  alignment _ = alignment (undefined :: CLong)
  poke p (StructTimestamp year month day hour minute second fraction) = do
    #{poke TIMESTAMP_STRUCT, year}      p year
    #{poke TIMESTAMP_STRUCT, month}     p month
    #{poke TIMESTAMP_STRUCT, day}       p day
    #{poke TIMESTAMP_STRUCT, hour}      p hour
    #{poke TIMESTAMP_STRUCT, minute}    p minute
    #{poke TIMESTAMP_STRUCT, second}    p second
    #{poke TIMESTAMP_STRUCT, fraction}  p fraction
  peek p = return StructTimestamp
    `ap` (#{peek TIMESTAMP_STRUCT, year}     p)
    `ap` (#{peek TIMESTAMP_STRUCT, month}    p)
    `ap` (#{peek TIMESTAMP_STRUCT, day}      p)
    `ap` (#{peek TIMESTAMP_STRUCT, hour}     p)
    `ap` (#{peek TIMESTAMP_STRUCT, minute}   p)
    `ap` (#{peek TIMESTAMP_STRUCT, second}   p)
    `ap` (#{peek TIMESTAMP_STRUCT, fraction} p)

-- | StructGUID
-- data StructGUID = StructGUID
--   #{type DWORD}     -- ^ Data1
--   #{type WORD}      -- ^ Data2
--   #{type WORD}      -- ^ Data3
--   [#{type BYTE}]    -- ^ Data4[8]
--
-- instance Storable StructGUID where
--   sizeOf _ = #{size SQLGUID}
--   alignment _ = alignment (undefined :: CLong)
--   poke p (StructGUID data1 data2 data3 data4) = do
--     #{poke SQLGUID, Data1} p data1
--     #{poke SQLGUID, Data2} p data2
--     #{poke SQLGUID, Data3} p data3
--     pokeArray (p `plusPtr` #{offset SQLGUID, Data4}) data4
--   peek p = return StructGUID
--     `ap` (#{peek SQLGUID, Data1} p)
--     `ap` (#{peek SQLGUID, Data2} p)
--     `ap` (#{peek SQLGUID, Data3} p)
--     `ap` (peekArray 8 (p `plusPtr` #{offset SQLGUID, Data4}))


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
mkBindCol :: SState -> SQLHSTMT -> #{type SQLSMALLINT} -> IO (BindCol, Ptr #{type SQLLEN})
mkBindCol sstate cstmt col = do
  hdbcTrace "mkBindCol"
  colInfo <- readMVar (colinfomv sstate)
  let colDesc = (snd (colInfo !! ((fromIntegral col) -1)))
  case colType colDesc of
    SqlCharT          -> mkBindColStringEC  cstmt col' (colSize colDesc)
    SqlVarCharT       -> mkBindColStringEC  cstmt col' (colSize colDesc)
    SqlLongVarCharT   -> mkBindColString    cstmt col' (colSize colDesc)
    SqlWCharT         -> mkBindColWStringEC cstmt col' (colSize colDesc)
    SqlWVarCharT      -> mkBindColWStringEC cstmt col' (colSize colDesc)
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
--    SqlIntervalT i    -> mkBindColInterval  cstmt col' (colSize colDesc) i
--    SqlGUIDT          -> mkBindColGUID      cstmt col' (colSize colDesc)
    _                 -> mkBindColGetData   col'
-- The following are not supported by ODBC:
--    SqlUTCDateTimeT
--    SqlUTCTimeT
--    SqlTimeWithZoneT
--    SqlTimestampWithZoneT
 where
  col' = fromIntegral col

colBufSizeDefault = 1024
colBufSizeMaximum = 4096

utf8EncodingMaximum = 6
wcSize = 2

-- The functions that follow do the marshalling from C into a Haskell type
mkBindColString cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColString"
  let colSize = min colBufSizeMaximum $ fromMaybe colBufSizeDefault mColSize
  let bufLen  = sizeOf (undefined :: CChar) * (colSize + 1)
  buf     <- mallocBytes bufLen
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_CHAR}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColString buf (fromIntegral bufLen) col, pStrLen)
mkBindColStringEC cstmt col = mkBindColString cstmt col . fmap (* utf8EncodingMaximum)
mkBindColWString cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColWString"
  let colSize = min colBufSizeMaximum $ fromMaybe colBufSizeDefault mColSize
  let bufLen  = sizeOf (undefined :: CWchar) * (colSize + 1)
  buf     <- mallocBytes bufLen
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_CHAR}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColWString buf (fromIntegral bufLen) col, pStrLen)
mkBindColWStringEC cstmt col = mkBindColString cstmt col . fmap extendFactor  where
  extendFactor sz = sz * ((utf8EncodingMaximum + wcSize - 1) `quot` wcSize)
mkBindColBit cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColBit"
  let bufLen  = sizeOf (undefined :: CChar)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_BIT}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColBit buf, pStrLen)
mkBindColTinyInt cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColTinyInt"
  let bufLen  = sizeOf (undefined :: CUChar)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_STINYINT}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColTinyInt buf, pStrLen)
mkBindColShort cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColShort"
  let bufLen  = sizeOf (undefined :: CShort)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_SSHORT}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColShort buf, pStrLen)
mkBindColLong cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColSize"
  let bufLen  = sizeOf (undefined :: CLong)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_SLONG}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColLong buf, pStrLen)
mkBindColBigInt cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColBigInt"
  let bufLen  = sizeOf (undefined :: CInt)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_SBIGINT}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColBigInt buf, pStrLen)
mkBindColFloat cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColFloat"
  let bufLen  = sizeOf (undefined :: CFloat)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_FLOAT}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColFloat buf, pStrLen)
mkBindColDouble cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColDouble"
  let bufLen  = sizeOf (undefined :: CDouble)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_DOUBLE}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColDouble buf, pStrLen)
mkBindColBinary cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColBinary"
  let colSize = min colBufSizeMaximum $ fromMaybe colBufSizeDefault mColSize
  let bufLen  = sizeOf (undefined :: CUChar) * (colSize + 1)
  buf     <- mallocBytes bufLen
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_BINARY}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColBinary buf (fromIntegral bufLen) col, pStrLen)
mkBindColDate cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColDate"
  let bufLen = sizeOf (undefined :: StructDate)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_TYPE_DATE}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColDate buf, pStrLen)
mkBindColTime cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColTime"
  let bufLen = sizeOf (undefined :: StructTime)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_TYPE_TIME}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColTime buf, pStrLen)
mkBindColTimestamp cstmt col mColSize = do
  hdbcTrace "mkBindCol: BindColTimestamp"
  let bufLen = sizeOf (undefined :: StructTimestamp)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_TYPE_TIMESTAMP}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColTimestamp buf, pStrLen)
mkBindColGetData col = do
  hdbcTrace "mkBindCol: BindColGetData"
  return (BindColGetData col, nullPtr)

freeBindCol :: BindCol -> IO ()
freeBindCol (BindColString   buf _ _) = free buf
freeBindCol (BindColWString  buf _ _) = free buf
freeBindCol (BindColBit      buf) = free buf
freeBindCol (BindColTinyInt  buf) = free buf
freeBindCol (BindColShort    buf) = free buf
freeBindCol (BindColLong     buf) = free buf
freeBindCol (BindColBigInt   buf) = free buf
freeBindCol (BindColFloat    buf) = free buf
freeBindCol (BindColDouble   buf) = free buf
freeBindCol (BindColBinary   buf _ _) = free buf
freeBindCol (BindColDate     buf) = free buf
freeBindCol (BindColTime     buf) = free buf
freeBindCol (BindColTimestamp buf) = free buf
freeBindCol (BindColGetData  _ )   = return ()

-- | This assumes that SQL_ATTR_MAX_LENGTH is set to zero, otherwise, we
-- cannot detect truncated columns. See "returning Data in Bound Columns":
--     http://msdn.microsoft.com/en-us/library/ms712424(v=vs.85).aspx
-- Also note that the strLen value of SQL_NTS denotes a null terminated string,
-- but is only valid as input, so we don't make use of it here:
--     http://msdn.microsoft.com/en-us/library/ms713532(v=VS.85).aspx
bindColToSqlValue :: SQLHSTMT -> (BindCol, Ptr #{type SQLLEN}) -> IO SqlValue
bindColToSqlValue pcstmt (BindColGetData col, _) = do
  hdbcTrace "bindColToSqlValue: BindColGetData"
  getColData pcstmt #{const SQL_CHAR} col
bindColToSqlValue pcstmt (bindCol, pStrLen) = do
  hdbcTrace "bindColToSqlValue"
  strLen <- peek pStrLen
  case strLen of
    #{const SQL_NULL_DATA} -> return SqlNull
    #{const SQL_NO_TOTAL}  -> getLongColData pcstmt bindCol
    _                      -> bindColToSqlValue' pcstmt bindCol strLen

-- | This is a worker function for `bindcolToSqlValue`. Note that the case
-- where the data is null should already be handled by this stage.
bindColToSqlValue' :: SQLHSTMT -> BindCol -> #{type SQLLEN} -> IO SqlValue
bindColToSqlValue' pcstmt (BindColString buf bufLen col) strLen
  | bufLen >= strLen = do
      bs <- B.packCStringLen (buf, fromIntegral strLen)
      hdbcTrace $ "bindColToSqlValue BindColString " ++ show bs ++ " " ++ show strLen
      return $ SqlByteString bs
  | otherwise = getColData pcstmt #{const SQL_CHAR} col
bindColToSqlValue' pcstmt (BindColWString buf bufLen col) strLen
  | bufLen >= strLen = do
      bs <- B.packCStringLen (castPtr buf, fromIntegral strLen)
      hdbcTrace $ "bindColToSqlValue BindColWString " ++ show bs ++ " " ++ show strLen
      return $ SqlByteString bs
  | otherwise = getColData pcstmt #{const SQL_CHAR} col
bindColToSqlValue' _ (BindColBit     buf) strLen = do
  bit <- peek buf
  hdbcTrace $ "bindColToSqlValue BindColBit " ++ show bit
  return $ SqlChar (castCUCharToChar bit)
bindColToSqlValue' _ (BindColTinyInt buf) strLen = do
  tinyInt <- peek buf
  hdbcTrace $ "bindColToSqlValue BindColTinyInt " ++ show tinyInt
  return $ SqlChar (castCCharToChar tinyInt)
bindColToSqlValue' _ (BindColShort   buf) strLen = do
  short <- peek buf
  hdbcTrace $ "bindColToSqlValue BindColShort" ++ show short
  return $ SqlInt32 (fromIntegral short)
bindColToSqlValue' _ (BindColLong    buf) strLen = do
  long <- peek buf
  hdbcTrace $ "bindColToSqlValue BindColLong " ++ show long
  return $ SqlInt32 (fromIntegral long)
bindColToSqlValue' _ (BindColBigInt  buf) strLen = do
  bigInt <- peek buf
  hdbcTrace $ "bindColToSqlValue BindColBigInt " ++ show bigInt
  return $ SqlInt64 (fromIntegral bigInt)
bindColToSqlValue' _ (BindColFloat   buf) strLen = do
  float <- peek buf
  hdbcTrace $ "bindColToSqlValue BindColFloat " ++ show float
  return $ SqlDouble (realToFrac float)
bindColToSqlValue' _ (BindColDouble  buf) strLen = do
  double <- peek buf
  hdbcTrace $ "bindColToSqlValue BindColDouble " ++ show double
  return $ SqlDouble (realToFrac double)
bindColToSqlValue' pcstmt (BindColBinary  buf bufLen col) strLen
  | bufLen >= strLen = do
      bs <- B.packCStringLen (castPtr buf, fromIntegral strLen)
      hdbcTrace $ "bindColToSqlValue BindColBinary " ++ show bs
      return $ SqlByteString bs
  | otherwise = getColData pcstmt (#{const SQL_C_BINARY}) col
bindColToSqlValue' _ (BindColDate buf) strLen = do
  StructDate year month day <- peek buf
  hdbcTrace $ "bindColToSqlValue BindColDate"
  return $ SqlLocalDate $ fromGregorian
    (fromIntegral year) (fromIntegral month) (fromIntegral day)
bindColToSqlValue' _ (BindColTime buf) strLen = do
  StructTime hour minute second <- peek buf
  hdbcTrace $ "bindColToSqlValue BindColTime"
  return $ SqlLocalTimeOfDay $ TimeOfDay
    (fromIntegral hour) (fromIntegral minute) (fromIntegral second)
bindColToSqlValue' _ (BindColTimestamp buf) strLen = do
  StructTimestamp year month day hour minute second nanosecond <- peek buf
  hdbcTrace $ "bindColToSqlValue BindColTimestamp"
  return $ SqlLocalTime $ LocalTime
    (fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day))
    (TimeOfDay (fromIntegral hour) (fromIntegral minute)
    (fromIntegral second + (fromIntegral nanosecond / 1000000000)))
bindColToSqlValue' _ (BindColGetData _) _ =
  error "bindColToSqlValue': unexpected BindColGetData!"

fgetcolinfo :: SQLHSTMT -> IO [(String, SqlColDesc)]
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

freeBoundCols :: SState -> IO ()
freeBoundCols sstate = modifyMVar_ (bindColsMV sstate) $ \maybeBindCols -> do
    F.mapM_ go maybeBindCols
    return Nothing
  where
    go bindCols = do
      hdbcTrace "freeBoundCols"
      mapM_ (\(bindCol, pSqlLen) -> freeBindCol bindCol >> free pSqlLen) bindCols

ffinish :: SState -> IO ()
ffinish sstate = do
  hdbcTrace "ffinish"
  withMaybeStmt (sstmt sstate) $ F.mapM_ $ \hStmt -> do
    c_sqlFreeStmt hStmt sQL_CLOSE >>= checkError "fexecute c_sqlFreeStmt sQL_CLOSE" (StmtHandle hStmt)
    c_sqlFreeStmt hStmt sQL_UNBIND >>= checkError "fexecute c_sqlFreeStmt sQL_UNBIND" (StmtHandle hStmt)
    c_sqlFreeStmt hStmt sQL_RESET_PARAMS >>= checkError "fexecute c_sqlFreeStmt sQL_RESET_PARAMS" (StmtHandle hStmt)
  freeBoundCols sstate

ffinalize :: SState -> IO ()
ffinalize sstate = do
  ffinish sstate
  freeStmtIfNotAlready $ sstmt sstate

foreign import #{CALLCONV} safe "sql.h SQLDescribeCol"
  sqlDescribeCol :: SQLHSTMT
                 -> #{type SQLSMALLINT} -- ^ Column number
                 -> CString     -- ^ Column name
                 -> #{type SQLSMALLINT} -- ^ Buffer length
                 -> Ptr (#{type SQLSMALLINT}) -- ^ name length ptr
                 -> Ptr (#{type SQLSMALLINT}) -- ^ data type ptr
                 -> Ptr (#{type SQLULEN}) -- ^ column size ptr
                 -> Ptr (#{type SQLSMALLINT}) -- ^ decimal digits ptr
                 -> Ptr (#{type SQLSMALLINT}) -- ^ nullable ptr
                 -> IO #{type SQLRETURN}

foreign import #{CALLCONV} safe "sql.h SQLGetData"
  sqlGetData :: SQLHSTMT       -- ^ statement handle
             -> #{type SQLUSMALLINT} -- ^ Column number
             -> #{type SQLSMALLINT} -- ^ target type
             -> CString -- ^ target value pointer (void * in C)
             -> #{type SQLLEN} -- ^ buffer len
             -> Ptr (#{type SQLLEN})
             -> IO #{type SQLRETURN}

foreign import #{CALLCONV} safe "sql.h SQLBindCol"
  sqlBindCol :: SQLHSTMT            -- ^ statement handle
             -> #{type SQLUSMALLINT} -- ^ Column number
             -> #{type SQLSMALLINT}  -- ^ target type
             -> Ptr ColBuf           -- ^ target value pointer (void * in C)
             -> #{type SQLLEN}       -- ^ buffer len
             -> Ptr (#{type SQLLEN}) -- ^ strlen_or_indptr
             -> IO #{type SQLRETURN}

foreign import #{CALLCONV} safe "sql.h SQLPrepare"
  sqlPrepare :: SQLHSTMT -> CString -> #{type SQLINTEGER}
             -> IO #{type SQLRETURN}

foreign import #{CALLCONV} safe "sql.h SQLExecute"
  sqlExecute :: SQLHSTMT -> IO #{type SQLRETURN}

foreign import #{CALLCONV} safe "sql.h SQLNumResultCols"
  sqlNumResultCols :: SQLHSTMT -> Ptr #{type SQLSMALLINT}
                   -> IO #{type SQLRETURN}

foreign import #{CALLCONV} safe "sql.h SQLRowCount"
  sqlRowCount :: SQLHSTMT -> Ptr #{type SQLINTEGER} -> IO #{type SQLRETURN}

foreign import #{CALLCONV} safe "sql.h SQLBindParameter"
  sqlBindParameter :: SQLHSTMT -- ^ Statement handle
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

foreign import ccall safe "hdbc-odbc-helper.h &nullDataHDBC"
  nullDataHDBC :: Ptr #{type SQLLEN}

foreign import #{CALLCONV} safe "sql.h SQLDescribeParam"
  sqlDescribeParam :: SQLHSTMT
                   -> #{type SQLUSMALLINT} -- ^ parameter number
                   -> Ptr #{type SQLSMALLINT} -- ^ data type ptr
                   -> Ptr #{type SQLULEN} -- ^ parameter size ptr
                   -> Ptr #{type SQLSMALLINT} -- ^ dec digits ptr
                   -> Ptr #{type SQLSMALLINT} -- ^ nullable ptr
                   -> IO #{type SQLRETURN}

foreign import #{CALLCONV} safe "sql.h SQLFetch"
  sqlFetch :: SQLHSTMT -> IO #{type SQLRETURN}

foreign import ccall safe "hdbc-odbc-helper.h simpleSqlTables"
  simpleSqlTables :: SQLHSTMT -> IO #{type SQLRETURN}

foreign import ccall safe "hdbc-odbc-helper.h simpleSqlColumns"
  simpleSqlColumns :: SQLHSTMT -> Ptr CChar ->
                      #{type SQLSMALLINT} -> IO #{type SQLRETURN}

fgetparminfo :: SQLHSTMT -> IO [SqlColDesc]
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

getNumParams :: SQLHSTMT -> IO Int16
getNumParams sthptr = alloca $ \pcount ->
    do sqlNumParams sthptr pcount >>= checkError "SQLNumResultCols"
                                          (StmtHandle sthptr)
       peek pcount

foreign import #{CALLCONV} safe "sql.h SQLNumParams"
  sqlNumParams :: SQLHSTMT -> Ptr #{type SQLSMALLINT}
               -> IO #{type SQLRETURN}
