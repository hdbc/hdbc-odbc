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

For the error "2013: Mysql server has gone away" error message, you'll have to
use withRTSSignalsBlocked from the HDBC-mysql package.

query conn stmStr binds = withRTSSignalsBlocked $ quickQuery conn stmStr binds

Getting Started
---------------

Here are some instructions to set up ODBC with a sqlite3 backend, and how
to communicate with that database with HDBC-ODBC.
These instructions are written to work with Ubuntu 11.10.

First, we'll need to install the appropriate libraries:

    sudo apt-get install unixodbc unixodbc-dev unixodbc-bin
    sudo apt-get install libsqliteodbc

Verify that the sqlite ODBC drivers have been set up correctly:

    odbcinst -q -d
This should return:

    [SQLite]
    [SQLite3]

Next, fire up the `ODBCConfig` too to set up a new DSN:

    ODBCConfig

If you want to run the HDBC test suite, then set your DSN to `hdbctest`,
and set up to connect to a database of your choice, such as an empty file
in the `hdbc-odbc/testsrc` directory:

    touch hdbc-odbc/testsrc/hdbctest.db

You can check that everything is working appropriately in ghci:

    ghci> :m + Database.HDBC Database.HDBC.ODBC
    ghci> conn <- connectODBC "DSN=hdbctest"
    ghci> hdbcDriverName conn
    "odbc"
    ghci> hdbcClientVer conn
    "03.52"

You can then run some tests on your database:

    cd testsrc
    runhaskell runtests.hs

Contributing
------------

Contributions are welcome! If you would like to contribute, please fork the the
[github repository](https://github.com/hdbc/hdbc-odbc), and submit a pull
request.
