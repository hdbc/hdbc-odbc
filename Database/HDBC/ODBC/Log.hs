{-# LANGUAGE OverloadedStrings #-}
module Database.HDBC.ODBC.Log
  ( hdbcLog
  , hdbcTrace
  , hdbcTraceLT
  ) where

import System.IO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

hdbcLog :: String -> IO ()
-- hdbcLog _ = return ()
hdbcLog m = hPutStrLn stderr ("\n" ++ m)

hdbcTrace :: String -> IO ()
-- hdbcTrace _ = return ()
hdbcTrace m = hPutStrLn stderr ("\n" ++ m)

hdbcTraceLT :: TL.Text -> IO ()
-- hdbcTraceLT _ = return ()
hdbcTraceLT m = TLIO.hPutStrLn stderr $ TL.concat ["\n", m]
