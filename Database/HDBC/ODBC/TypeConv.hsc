-- -*- mode: haskell; -*-
{-# CFILES hdbc-odbc-helper.c #-}
-- Above line for hugs
{-
Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

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
module Database.HDBC.ODBC.TypeConv where
import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.DriverUtils
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

fromOType (_:_:_:colname:datatype:_:colsize:buflen:decdig:precrad:nullable:_:_:_:subtype:octetlen:_) =
    (fromSql colname,
     SqlColDesc {colType = convdatatype (fromSql datatype),
                 colSize = Just (fromSql colsize),
                 colOctetLength = Nothing,
                 colDecDigits = Nothing,
                 colNullable = case fromSql nullable of
                                 #{const SQL_NO_NULLS} -> Just False,
                                 #{const SQL_NULLABLE} -> Just True,
                                 _ -> Nothing
                }
    )

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
      #{const SQL_NUMBERIC} -> SqlNumericT
      #{const SQL_SMALLINT} -> SqlSmallIntT
      #{const SQL_INTEGER} -> SqlIntegerT
      #{const SQL_REAL} -> SqlRealT
      #{const SQL_FLOAT} -> SqlFloatT
      #{const SQL_DOUBLE} -> SqlDoubleT
      #{const SQL_BIT} -> SqlBitT
      #{const SQL_TINYINT} -> SqlTinyIntT
      #{const SQL_BIGINST} -> SqlBigIntT
      #{const SQL_BINARY} -> SqlBinaryT
      #{const SQL_VARBINARY} -> SqlVarBinaryT
      #{const SQL_LONGVARBINARY} -> SqlLongVarBinaryT
      #{const SQL_TYPE_DATE} -> SqlDateT
      #{const SQL_TYPE_TIME} -> SqlTimeT
      #{const SQL_TYPE_TIMESTAMP} -> SqlTimestampT
      #{const SQL_TYPE_UTCDATETIME} -> SqlUTCDateTimeT
      #{const SQL_TYPE_UTCTIME} -> SqlUTCTimeT
      #{const SQL_INTERVAL_MONTH} -> SqlInvervalT SqlIntervalMonthT
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
      #{const SQL_INTERVAL_MINUTE_TO_SECOND} -> SqlIntervalT SqlIntervalMinutetoSecondT
      #{const SQL_GUID} -> SqlGUIDT
      x -> SqlUnknownT (show x)
