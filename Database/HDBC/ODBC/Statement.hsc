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
                               colInfo <- readMVar (colinfomv sstate)
                               res <- mapM (getColData cstmt colInfo) [1..ncols]
                               return (stmt, Just res)

-- TODO: This code does not deal well with data that is extremely large,
-- where multiple fetches are required.
getColData cstmt colInfo icol = do
  let defaultLen = 128
  l $ "getColData: colInfo is " ++ show colInfo ++ ", icol " ++ show icol
  let cBinding = case colType (snd (colInfo !! ((fromIntegral icol) - 1))) of
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

-- TODO: The SQL_SUCCESS_WITH_INFO currenty delegates to the old behaviour,
-- better would be to use getColData on only the buffers which have been
-- truncated (ie String and Binary types).
ffetchrowBindCol :: SState -> IO (Maybe [SqlValue])
ffetchrowBindCol sstate = modifyMVar (stomv sstate) $ \stmt ->
  case stmt of
    Nothing -> do
      l "ffetchrowBindCol"
      return (stmt, Nothing)
    Just cmstmt -> withStmt cmstmt $ \cstmt -> do
      colInfo <- readMVar (colinfomv sstate)
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
          sqlValues <- case rc of
            #{const SQL_SUCCESS}           -> mapM (bindColToSqlValue) bindCols
            #{const SQL_SUCCESS_WITH_INFO} -> do
              ncols <- getNumResultCols cstmt
              l $ "ncols: " ++ show ncols
              colInfo <- readMVar (colinfomv sstate)
              mapM (getColData cstmt colInfo) [1..ncols]
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

-- These correspond to the C type identifiers found here:
--     http://msdn.microsoft.com/en-us/library/ms714556(v=VS.85).aspx
-- The Ptr values point to the appropriate C types
data BindCol
  = BindColString  (Ptr CChar)
  | BindColWString (Ptr CWchar)
  | BindColBit     (Ptr CUChar)
  | BindColTinyInt (Ptr CChar)
  | BindColShort   (Ptr CShort)
  | BindColLong    (Ptr CLong)
  | BindColBigInt  (Ptr CInt)    -- TODO: _int64
  | BindColFloat   (Ptr CFloat)
  | BindColDouble  (Ptr CDouble)
  | BindColBinary  (Ptr CUChar)
  | BindColUnknown (Ptr CChar)
  | BindColDate    (Ptr StructDate)
  | BindColTime    (Ptr StructTime)
  | BindColTimestamp (Ptr StructTimestamp)
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
  | BindColGUID (Ptr StructGUID)


-- | StructDate is used to marshal the DATE_STRUCT
-- This struct, and the ones which follow, are described here:
--     http://msdn.microsoft.com/en-us/library/ms714556(v=VS.85).aspx
data StructDate = StructDate
  #{type SQLSMALLINT}   -- ^ year
  #{type SQLUSMALLINT}  -- ^ month
  #{type SQLUSMALLINT}  -- ^ day

instance Storable StructDate where
  sizeOf _    = #{size DATE_STRUCT}
  alignment _ = alignment (undefined :: CInt) -- TODO: check this is correct
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
  #{type SQLUSMALLINT} -- ^ hour
  #{type SQLUSMALLINT} -- ^ minute
  #{type SQLUSMALLINT} -- ^ second

instance Storable StructTime where
  sizeOf _    = #{size TIME_STRUCT}
  alignment _ = alignment (undefined :: CInt) -- TODO: check this is correct
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
  #{type SQLSMALLINT}   -- ^ year
  #{type SQLUSMALLINT}  -- ^ month
  #{type SQLUSMALLINT}  -- ^ day
  #{type SQLUSMALLINT}  -- ^ hour
  #{type SQLUSMALLINT}  -- ^ minute
  #{type SQLUSMALLINT}  -- ^ second
  #{type SQLUINTEGER}   -- ^ fraction

instance Storable StructTimestamp where
  sizeOf _    = #{size TIMESTAMP_STRUCT}
  alignment _ = alignment (undefined :: CInt) -- TODO: check this is correct
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
-- TODO: Check the peek/poke of the struct member that's an array
data StructGUID = StructGUID
  #{type DWORD}     -- ^ Data1
  #{type WORD}      -- ^ Data2
  #{type WORD}      -- ^ Data3
  [#{type BYTE}]    -- ^ Data4[8]

instance Storable StructGUID where
  sizeOf _ = #{size SQLGUID}
  alignment _ = alignment (undefined :: CInt) -- TODO: check this is correct
  poke p (StructGUID data1 data2 data3 data4) = do
    #{poke SQLGUID, Data1} p data1
    #{poke SQLGUID, Data2} p data2
    #{poke SQLGUID, Data3} p data3
    pokeArray (p `plusPtr` #{offset SQLGUID, Data4}) data4
  peek p = return StructGUID
    `ap` (#{peek SQLGUID, Data1} p)
    `ap` (#{peek SQLGUID, Data2} p)
    `ap` (#{peek SQLGUID, Data3} p)
    `ap` (peekArray 8 (p `plusPtr` #{offset SQLGUID, Data4}))


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
  sqlBindCol cstmt col (#{const SQL_C_CHAR}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColString buf, pStrLen)
mkBindColWString cstmt col mColSize = do
  let colSize = fromMaybe 128 mColSize
  let bufLen  = sizeOf (undefined :: CWchar) * colSize
  buf     <- mallocBytes bufLen
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_CHAR}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColWString buf, pStrLen)
mkBindColBit cstmt col mColSize = do
  let bufLen  = sizeOf (undefined :: CChar)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_BIT}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColBit buf, pStrLen)
mkBindColTinyInt cstmt col mColSize = do
  let bufLen  = sizeOf (undefined :: CUChar)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_STINYINT}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColTinyInt buf, pStrLen)
mkBindColShort cstmt col mColSize = do
  let bufLen  = sizeOf (undefined :: CShort)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_SSHORT}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColShort buf, pStrLen)
mkBindColLong cstmt col mColSize = do
  let bufLen  = sizeOf (undefined :: CLong)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_SLONG}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColLong buf, pStrLen)
mkBindColBigInt cstmt col mColSize = do
  let bufLen  = sizeOf (undefined :: CInt)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_SBIGINT}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColBigInt buf, pStrLen)
mkBindColFloat cstmt col mColSize = do
  let bufLen  = sizeOf (undefined :: CFloat)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_FLOAT}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColBit buf, pStrLen)
mkBindColDouble cstmt col mColSize = do
  let bufLen  = sizeOf (undefined :: CDouble)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_DOUBLE}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColBit buf, pStrLen)
mkBindColBinary cstmt col mColSize = do
  let colSize = fromMaybe 128 mColSize
  let bufLen  = sizeOf (undefined :: CUChar) * colSize
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_BINARY}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColBit buf, pStrLen)
mkBindColDate cstmt col mColSize = do
  let bufLen = sizeOf (undefined :: StructDate)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_TYPE_DATE}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColDate buf, pStrLen)
mkBindColTime cstmt col mColSize = do
  let bufLen = sizeOf (undefined :: StructTime)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_TYPE_TIME}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColTime buf, pStrLen)
mkBindColTimestamp cstmt col mColSize = do
  let bufLen = sizeOf (undefined :: StructTimestamp)
  buf     <- malloc
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_TYPE_TIMESTAMP}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColTimestamp buf, pStrLen)
mkBindColInterval cstmt col mColsize interval = mkBindColUnknown cstmt col mColsize "Interval"
mkBindColGUID cstmt col mColsize = mkBindColUnknown cstmt col mColsize "GUID"
mkBindColUnknown cstmt col mColSize str = do
  let bufLen = 128
  buf     <- mallocBytes bufLen
  pStrLen <- malloc
  sqlBindCol cstmt col (#{const SQL_C_CHAR}) (castPtr buf) (fromIntegral bufLen) pStrLen
  return (BindColUnknown buf, pStrLen)

freeBindCol :: BindCol -> IO ()
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
freeBindCol (BindColDate     buf) = free buf
freeBindCol (BindColTime     buf) = free buf
freeBindCol (BindColTimestamp buf) = free buf

-- | This assumes that SQL_ATTR_MAX_LENGTH is set to zero, otherwise, we
-- cannot detect truncated columns. See "returning Data in Bound Columns":
--     http://msdn.microsoft.com/en-us/library/ms712424(v=vs.85).aspx
-- Also note that the strLen value of SQL_NTS denotes a null terminated string,
-- but is only valid as input, so we don't make use of it here:
--     http://msdn.microsoft.com/en-us/library/ms713532(v=VS.85).aspx
bindColToSqlValue :: (BindCol, Ptr #{type SQLLEN}) -> IO SqlValue
bindColToSqlValue (bindCol, pStrLen) = do
  strLen <- peek pStrLen
  case strLen of
    #{const SQL_NULL_DATA} -> return SqlNull
    #{const SQL_NO_TOTAL}  -> fail $ "Unexpected SQL_NO_TOTAL"
    _                      -> bindColToSqlValue' bindCol strLen

bindColToSqlValue' :: BindCol -> #{type SQLLEN} -> IO SqlValue
bindColToSqlValue' (BindColString buf) strLen = do
  bs <- B.packCStringLen (buf, fromIntegral strLen)
  return $ SqlByteString bs
bindColToSqlValue' (BindColWString buf) strLen = do
  bs <- B.packCStringLen (castPtr buf, fromIntegral strLen)
  return $ SqlByteString bs
bindColToSqlValue' (BindColUnknown buf) strLen = do
  bs <- B.packCStringLen (buf, fromIntegral strLen)
  return $ SqlByteString bs
bindColToSqlValue' (BindColBit     buf) strLen = do
  bit <- peek buf
  return $ SqlChar (castCUCharToChar bit)
bindColToSqlValue' (BindColTinyInt buf) strLen = do
  tinyInt <- peek buf
  return $ SqlChar (castCCharToChar tinyInt)
bindColToSqlValue' (BindColShort   buf) strLen = do
  short <- peek buf
  return $ SqlInt32 (fromIntegral short)
bindColToSqlValue' (BindColLong    buf) strLen = do
  long <- peek buf
  return $ SqlInt32 (fromIntegral long)
bindColToSqlValue' (BindColBigInt  buf) strLen = do
  bigInt <- peek buf
  return $ SqlInt64 (fromIntegral bigInt)
bindColToSqlValue' (BindColFloat   buf) strLen = do
  float <- peek buf
  return $ SqlDouble (realToFrac float)
bindColToSqlValue' (BindColDouble  buf) strLen = do
  double <- peek buf
  return $ SqlDouble (realToFrac double)
bindColToSqlValue' (BindColBinary  buf) strLen = do
  bs <- B.packCStringLen (castPtr buf, fromIntegral strLen)
  return $ SqlByteString bs

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
public_ffinish sstate = do
  l "public_ffinish"
  modifyMVar_ (stomv sstate) freeMStmt
  modifyMVar_ (bindColsMV sstate) freeBindCols
 where
  freeMStmt Nothing    = return Nothing
  freeMStmt (Just sth) = ffinish sth >> return Nothing
  freeBindCols bindCols = do
    mapM_ (\(bindCol, pSqlLen) -> freeBindCol bindCol >> free pSqlLen) bindCols
    return []

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
