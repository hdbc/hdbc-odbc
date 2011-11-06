HDBC-ODBC
=========

Welcome to HDBC, Haskell Database Connectivity.

This package provides a database backend driver for ODBC.  You should
be able to use any ODBC front-end with it.

Please see HDBC itself for documentation on use.

This package provides one function in module Database.HDBC.ODBC:

    -- | Connect to an ODBC server.
    --   For information on the meaning of the passed string, please see:
    -- <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/odbc/htm/odbcsqldrivers.asp>
    connectODBC :: String -> IO Connection

For example, you might use `connectODBC` as follows:

    connectODBC "DSN=hdbctest1"

For more information about HDBC-ODBC,
please visit the [wiki](https://github.com/hdbc/hdbc-odbc/wiki).

Differences from HDBC standard
------------------------------

None known at this time.

MySQL note
----------

Important note for MySQL users:

Unless you are going to use InnoDB tables, you are strongly encouraged to set

    Option = 262144

in your odbc.ini (for Unix users), or to disable transaction support in your
DSN setup for Windows users.

If you fail to do this, the MySQL ODBC driver will incorrectly state that it
supports transactions.  dbTransactionSupport will incorrectly return True.
commit and rollback will then silently fail.  This is certainly *NOT* what you
want.  It is a bug (or misfeature) in the MySQL driver, not in HDBC.

You should ignore this advice if you are using InnoDB tables.

Contributing
------------

Contributions are welcome! If you would like to contribute, please fork the the
[github repository](https://github.com/hdbc/hdbc-odbc), and submit a pull
request.
