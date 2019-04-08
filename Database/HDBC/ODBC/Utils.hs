{- -*- mode: haskell; -*-
-}
module Database.HDBC.ODBC.Utils where
import Foreign.Ptr
import Control.Exception
import Foreign.Marshal.Array

withAnyArr0 :: (a -> IO (Ptr b)) -- ^ Function that transforms input data into pointer
            -> (Ptr b -> IO ())  -- ^ Function that frees generated data
            -> [a]               -- ^ List of input data
            -> (Ptr (Ptr b) -> IO c) -- ^ Action to run with the C array
            -> IO c             -- ^ Return value
withAnyArr0 input2ptract freeact inp action =
    bracket (mapM input2ptract inp)
            (\clist -> mapM_ freeact clist)
            (\clist -> withArray0 nullPtr clist action)
