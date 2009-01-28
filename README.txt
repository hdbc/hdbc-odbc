Welcome to HDBC, Haskell Database Connectivity.

This package provides a database backend driver for ODBC.  You should
be able to use any ODBC front-end with it.

Please see HDBC itself for documentation on use.  If you don't already
have it, you can browse this documentation at
http://darcs.complete.org/hdbc/doc/index.html.

This package provides one function in module Database.HDBC.ODBC:

{- | Connect to an ODBC server.

For information on the meaning of the passed string, please see:

<http://msdn.microsoft.com/library/default.asp?url=/library/en-us/odbc/htm/odbcsqldrivers.asp>

An example string is:

>"DSN=hdbctest1"

-}
connectODBC :: String -> IO Connection

DIFFERENCES FROM HDBC STANDARD
------------------------------

None known at this time.

MYSQL NOTE
----------

Important note for MySQL users:

Unless you are going to use InnoDB tables, you are strongly encouraged to set

Option = 262144

in your odbc.ini (for Unix users), or to disable transaction support in your
DSN setup for Windows users.

If you fail to do this, the MySQL ODBC driver will incorrectly state that it
supports transactions.  dbTransactionSupport will incorrectly return True.
commit and rollback will then silently fail.  This is certainly /NOT/ what you
want.  It is a bug (or misfeature) in the MySQL driver, not in HDBC.

You should ignore this advice if you are using InnoDB tables.

PREREQUISITES
-------------

Before installing this package, you'll need to have HDBC 2.0.0
above installed.  You can download HDBC from
http://software.complete.org/hdbc

This version of HDBC-ODBC requires GHC 6.8.x or above, or Hugs 2005xx
or above.  If you're using Hugs, you'll also need cpphs (see
http://www.cs.york.ac.uk/fp/cpphs/ if you don't already have it).

Older versions of this package will work with older versions of the
compiler toolchain.

INSTALLATION
------------

The steps to install are:

1) Examine HDBC-odbc.cabal and edit the include-dirs
   line to point to your local ODBC installation.  If necessary,
   uncomment and edit the extra-lib-dirs line to point to your
   local ODBC installation.

2) ghc --make -o setup Setup.lhs

3) ./setup configure

4) ./setup build

5) ./setup install   (as root)

If you're on Windows, you can omit the leading "./".

USAGE
-----

To use with hugs, you'll want to use hugs -98.

To use with GHC, you'll want to use:

 -package HDBC -package HDBC-odbc

Or, with Cabal, use:

  Build-Depends: HDBC>=2.0.0, HDBC-odbc

This package has been tested with unixODBC.

-- John Goerzen
   January 2009
   January 2006

