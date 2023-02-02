module Parsing.Args where
import System.Exit ( exitSuccess )
import System.Console.GetOpt
import System.IO ( hPutStrLn, stderr )
import Data.Maybe (fromMaybe)

data Options = Options {
    file    :: Maybe FilePath,
    help    :: Bool,
    debug   :: Bool
} deriving  Show

defaultOptions :: Options
defaultOptions = Options {
    file       = Nothing,
    help       = False,
    debug      = False
}


options :: [OptDescr (Options -> Options)]
options =
    [
        Option ['f'] ["file"]  (OptArg ((\ f opts -> opts { file = Just f }) . fromMaybe "stdin") "FILE") "The FILE that is to be executed.",
        Option ['h'] ["help"] (NoArg (\ opts -> opts { help = True })) "Display this message.",
        Option ['d'] ["debug"] (NoArg (\ opts -> opts { debug = True })) "Enter debug mode"
    ]

parse :: [String] -> IO (Options, [String])
parse argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> do
            if "-h" `elem` argv
                then do 
                    hPutStrLn stderr (usageInfo header options)
                    exitSuccess
                else
                    return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: glados [...options] < file.scm"

