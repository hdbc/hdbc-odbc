module SpecificDB where
import Database.HDBC
import Database.HDBC.ODBC
import Test.HUnit

connectDB = 
    handleSqlError (connectODBC "DSN=hdbctest")


-- These are copied from PostgreSQL for now, except for interval
dateTimeTypeOfSqlValue :: SqlValue -> String
dateTimeTypeOfSqlValue (SqlLocalDate _) = "date"
dateTimeTypeOfSqlValue (SqlLocalTimeOfDay _) = "time without time zone"
dateTimeTypeOfSqlValue (SqlZonedLocalTimeOfDay _ _) = "time with time zone"
dateTimeTypeOfSqlValue (SqlLocalTime _) = "timestamp without time zone"
dateTimeTypeOfSqlValue (SqlZonedTime _) = "timestamp with time zone"
dateTimeTypeOfSqlValue (SqlUTCTime _) = "timestamp with time zone"
dateTimeTypeOfSqlValue (SqlDiffTime _) = "numeric"
dateTimeTypeOfSqlValue (SqlPOSIXTime _) = "numeric"
dateTimeTypeOfSqlValue (SqlEpochTime _) = "integer"
dateTimeTypeOfSqlValue (SqlTimeDiff _) = "numeric"
dateTimeTypeOfSqlValue _ = "text"

supportsFracTime = True
