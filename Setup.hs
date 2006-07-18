import Distribution.Simple
import qualified System.Info
import Data.List
import Distribution.Simple.Utils
import Distribution.PackageDescription
import System.Exit

main = defaultMainWithHooks defaultUserHooks{preConf = conf, postConf = ok}
       where ok _ _ _ _ = return ExitSuccess

conf args flags =
    do config <- if isWindows
                 then do putStrLn "On Windows -- using odbc32"
                         return (emptyBuildInfo {extraLibs = ["odbc32"]})
                 else do putStrLn "Not on Windows -- using odbc"
                         return (emptyBuildInfo {extraLibs = ["odbc"]})
       writeHookedBuildInfo "HDBC-odbc.buildinfo" (Just config, [])
       return (Just config, [])

    where isWindows = isPrefixOf "mingw" System.Info.os
