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
    dir <- getCurrentDirectory
    print (dir ++ "/c")
    return localBuildInfo {
        localPkgDescr = myPackageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = (dir ++ "/c") :
                        extraLibDirs libBuild
                }
            }
        }
    }

copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    let verbosity = fromFlag $ copyVerbosity flags
    rawSystemExit verbosity "cp" ["c/libCacBDD.a", libPref]

cleanExtLib :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanExtLib _ flags _ _ =
    let verbosity = fromFlag $ cleanVerbosity flags
    in do
      rawSystemExit verbosity "env" ["make", "--directory=c", "clean"]
      rawSystemExit verbosity "env" ["make", "--directory=cpp", "clean"]
