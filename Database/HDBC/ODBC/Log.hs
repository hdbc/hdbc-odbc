{-# LANGUAGE OverloadedStrings #-}
module Database.HDBC.ODBC.Log
  ( hdbcLog
  , hdbcTrace
  ) where

import System.IO

hdbcLog :: String -> IO ()
hdbcLog _ = return ()
-- hdbcLog m = hPutStrLn stderr ("\n" ++ m)

hdbcTrace :: String -> IO ()
hdbcTrace _ = return ()
-- hdbcTrace m = hPutStrLn stderr ("\n" ++ m)
