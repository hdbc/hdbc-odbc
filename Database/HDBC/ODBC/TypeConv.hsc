-- -*- mode: haskell; -*-
{-# CFILES hdbc-odbc-helper.c #-}
-- Above line for hugs
module Database.HDBC.ODBC.TypeConv(fromOTypeInfo, fromOTypeCol) where
import Database.HDBC.Types
import Database.HDBC
import Data.Word
import Data.Int

l _ = return ()
-- l m = hPutStrLn stderr ("\n" ++ m)

#ifdef mingw32_HOST_OS
#include <windows.h>
#endif
#include <sql.h>
#include <sqlext.h>
#include <sqlucode.h>

fromOTypeInfo :: String         -- ^ Column name
              -> #{type SQLSMALLINT} -- ^ Data type
              -> #{type SQLULEN}     -- ^ Column size
              -> #{type SQLSMALLINT} -- ^ Is it nullable
              -> (String, SqlColDesc)
fromOTypeInfo colname datatype colsize nullable =
    (colname,
     SqlColDesc {colType = convdatatype datatype,
                 colOctetLength = Nothing,
                 colDecDigits = Nothing,
                 colSize = Just (fromIntegral colsize),
                 colNullable = case nullable of
                                 #{const SQL_NO_NULLS} -> Just False
                                 #{const SQL_NULLABLE} -> Just True
                                 _ -> Nothing
                }
    )

fromOTypeCol (_:_:_:colname:datatype:_:colsize:buflen:decdig:precrad:nullable:_:_:_:subtype:octetlen:_) =
    fromOTypeInfo (fromSql colname)
                  (fromIntegral ((fromSql datatype)::Int))
                  (fromSql colsize)
                  (fromIntegral ((fromSql nullable)::Int))
fromOTypeCol x = error $ "fromOTypeCol: unexpected result set: " ++ show x

convdatatype :: #{type SQLSMALLINT} -> SqlTypeId
convdatatype intype =
    case intype of
      #{const SQL_CHAR} -> SqlCharT
      #{const SQL_VARCHAR} -> SqlVarCharT
      #{const SQL_LONGVARCHAR} -> SqlLongVarCharT
      #{const SQL_WCHAR} -> SqlWCharT
      #{const SQL_WVARCHAR} -> SqlWVarCharT
      #{const SQL_WLONGVARCHAR} -> SqlWLongVarCharT
      #{const SQL_DECIMAL} -> SqlDecimalT
      #{const SQL_NUMERIC} -> SqlNumericT
      #{const SQL_SMALLINT} -> SqlSmallIntT
      #{const SQL_INTEGER} -> SqlIntegerT
      #{const SQL_REAL} -> SqlRealT
      #{const SQL_FLOAT} -> SqlFloatT
      #{const SQL_DOUBLE} -> SqlDoubleT
      #{const SQL_BIT} -> SqlBitT
      #{const SQL_TINYINT} -> SqlTinyIntT
      #{const SQL_BIGINT} -> SqlBigIntT
      #{const SQL_BINARY} -> SqlBinaryT
      #{const SQL_VARBINARY} -> SqlVarBinaryT
      #{const SQL_LONGVARBINARY} -> SqlLongVarBinaryT
      #{const SQL_TYPE_DATE} -> SqlDateT
      #{const SQL_TYPE_TIME} -> SqlTimeT
      #{const SQL_TYPE_TIMESTAMP} -> SqlTimestampT
      -- ODBC libraries don't seem to define the UTC items
       -- {const SQL_TYPE_UTCDATETIME} -> SqlUTCDateTimeT
       -- {const SQL_TYPE_UTCTIME} -> SqlUTCTimeT
      #{const SQL_INTERVAL_MONTH} -> SqlIntervalT SqlIntervalMonthT
      #{const SQL_INTERVAL_YEAR} -> SqlIntervalT SqlIntervalYearT
      #{const SQL_INTERVAL_YEAR_TO_MONTH} -> SqlIntervalT SqlIntervalYearToMonthT
      #{const SQL_INTERVAL_DAY} -> SqlIntervalT SqlIntervalDayT
      #{const SQL_INTERVAL_HOUR} -> SqlIntervalT SqlIntervalHourT
      #{const SQL_INTERVAL_MINUTE} -> SqlIntervalT SqlIntervalMinuteT
      #{const SQL_INTERVAL_SECOND} -> SqlIntervalT SqlIntervalSecondT
      #{const SQL_INTERVAL_DAY_TO_HOUR} -> SqlIntervalT SqlIntervalDayToHourT
      #{const SQL_INTERVAL_DAY_TO_MINUTE} -> SqlIntervalT SqlIntervalDayToMinuteT
      #{const SQL_INTERVAL_DAY_TO_SECOND} -> SqlIntervalT SqlIntervalDayToSecondT
      #{const SQL_INTERVAL_HOUR_TO_MINUTE} -> SqlIntervalT SqlIntervalHourToMinuteT
      #{const SQL_INTERVAL_HOUR_TO_SECOND} -> SqlIntervalT SqlIntervalHourToSecondT
      #{const SQL_INTERVAL_MINUTE_TO_SECOND} -> SqlIntervalT SqlIntervalMinuteToSecondT
      #{const SQL_GUID} -> SqlGUIDT
      x -> SqlUnknownT (show x)
