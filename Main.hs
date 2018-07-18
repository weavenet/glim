-- File: Main.hs

{-# LANGUAGE BangPatterns #-}

import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO
import Control.Monad

import Glim

version = "1.0.0"

data Options = Options  { optVerbose :: Bool }

startOptions :: Options
startOptions = Options  { optVerbose = False }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"
 
    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr version
                exitWith ExitSuccess))
        "Print version"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

main :: IO ()
main = do
    args <- getArgs
    let (actions, files, _) = getOpt Permute options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optVerbose = verbose } = opts

    !inputFromStdIn <- getContents

    putStrLn $ format $ process inputFromStdIn
