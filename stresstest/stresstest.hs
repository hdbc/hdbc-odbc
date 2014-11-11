{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Database.HDBC
import Data.Pool
import System.IO
import System.Random

import Database.HDBC.ODBC

connectionString :: String
connectionString = "DSN=hdbctest"

threadNumber :: Int
threadNumber = 4

prepareDatabase :: Pool ConnWrapper -> IO ()
prepareDatabase pool = withResource pool $ \conn -> withTransaction conn $ \tran ->
  void $ run tran
    "if not exists (select * from sysobjects where name='test_table' and xtype='U') \
    \create table test_table ( \
      \id int identity not null, \
      \value nvarchar(64) not null \
    \)" []

writerThread :: Pool ConnWrapper -> IO ()
writerThread pool = forever $ withResource pool $ \conn -> do
  len <- randomRIO (1, 64)
  value <- replicateM len $ randomRIO ('a', 'z')
  void $ do
    run conn "insert into test_table (value) values (?)" [SqlString value]
    commit conn

readerThread :: Pool ConnWrapper -> IO ()
readerThread pool = forever $ withResource pool $ \conn -> forever $ do
  !offset <- randomRIO (1, 100000) :: IO Int
  !limit <- randomRIO (1, 60) :: IO Int
  sth <- prepare conn $ "select * from test_table order by id offset " ++ show offset ++ " rows fetch next " ++ show limit ++ " rows only"
  execute sth []
  results <- fetchAllRows' sth
  finish sth
  return ()

main :: IO ()
main = handleSqlError $ bracket (createPool (ConnWrapper <$> connectODBC connectionString) disconnect 1 20 2) destroyAllResources $ \pool -> do
  prepareDatabase pool
  forkIO $ writerThread pool
  replicateM_ threadNumber . forkIO $
    readerThread pool
  hSetBuffering stdin NoBuffering
  void getChar
