module Main where

import Data.Maybe
import Distribution.PackageDescription    hiding (Flag)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import System.Directory

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  {
    preConf = makeExtLib
  , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
  , postConf = disablePostConfHooks -- seems crucial to not reset extraLibDir
  , preBuild = updateLibDirs
  , postCopy = copyExtLib
  , postClean = cleanExtLib
  }

makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
    let verbosity = fromFlag $ configVerbosity flags
    rawSystemExit verbosity "env" ["make", "--directory=cpp"]
    rawSystemExit verbosity "env" ["make", "--directory=c"]
    return emptyHookedBuildInfo

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let myPackageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library myPackageDescription
        libBuild = libBuildInfo lib
        libPref = libdir $ absoluteInstallDirs myPackageDescription localBuildInfo NoCopyDest
    putStrLn $ "NOTE confHook: adding " ++ libPref ++ " to extraLibDirs"
    return localBuildInfo {
        localPkgDescr = myPackageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = libPref : extraLibDirs libBuild
                }
            }
        }
    }

disablePostConfHooks :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
disablePostConfHooks args flags pd lbi = return ()

updateLibDirs :: Args -> BuildFlags -> IO HookedBuildInfo
updateLibDirs _ _ = do
    dir <- getCurrentDirectory
    putStrLn $ "NOTE preBuild: adding " ++ dir ++ "/c into extraLibDirs"
    let extlibDir = dir ++ "/c"
        bi = emptyBuildInfo { extraLibDirs = [ extlibDir ] }
    return (Just bi, [])

copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    let verbosity = fromFlag $ copyVerbosity flags
    installExecutableFile verbosity "c/libCacBDD.a" (libPref ++ "/libCacBDD.a")

cleanExtLib :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanExtLib _ flags _ _ =
    let verbosity = fromFlag $ cleanVerbosity flags
    in do
      rawSystemExit verbosity "env" ["make", "--directory=c", "clean"]
      rawSystemExit verbosity "env" ["make", "--directory=cpp", "clean"]
