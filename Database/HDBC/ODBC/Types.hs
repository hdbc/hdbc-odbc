module Database.HDBC.ODBC.Types
where

import Foreign.ForeignPtr
import Foreign

-- This may be wrong -- is SqlHandle always a pointer to something?
-- but it works with hsql so I'm going to use it here until I hear of it
-- breaking.
--newtype SqlHandle = Ptr ()

data CEnv = CEnv
type WrappedCEnv = Ptr CEnv
type Env = ForeignPtr WrappedCEnv

data CConn = CConn
type WrappedCConn = Ptr CConn
type Conn = ForeignPtr WrappedCConn

data CStmt = CStmt
type WrappedCStmt = Ptr CStmt
type Stmt = ForeignPtr WrappedCStmt


