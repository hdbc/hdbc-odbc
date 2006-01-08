module SpecificDB where
import Database.HDBC
import Database.HDBC.ODBC
import Test.HUnit

connectDB = 
    handleSqlError (connectODBC "testdb")
