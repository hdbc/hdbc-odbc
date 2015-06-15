{- |
   Module     : Database.HDBC.ODBC
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

HDBC driver interface for ODBC 3.x

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC.ODBC
    (
     connectODBC, Connection(), getQueryInfo, setAutoCommit
    )

where

import Database.HDBC.ODBC.Connection(connectODBC, Connection())
import Database.HDBC.ODBC.ConnectionImpl(getQueryInfo, setAutoCommit)
