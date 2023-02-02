module Parsing.Args where
import System.Exit ( exitSuccess )
import System.Console.GetOpt
import System.IO ( hPutStrLn, stderr )
import Data.Maybe (fromMaybe)


-- to access: (key name) (var name)
-- for example with Record called A we want to access field "file"
-- let filename = file A
data Args = Args {
    file  :: Maybe FilePath,
    help  :: Bool,
    debug :: Bool
} deriving  (Show, Eq)

defaultOptions :: Args
defaultOptions = Args {
    file       = Nothing,
    help       = False,
    debug      = False
}

options :: [OptDescr (Args -> Args)]
options =
    [
        Option ['f'] ["file"]  (OptArg ((\ f opts -> opts { file = Just f }) . fromMaybe "stdin") "FILE") "The FILE that is to be executed.",
        Option ['h'] ["help"] (NoArg (\ opts -> opts { help = True })) "Display this message.",
        Option ['d'] ["debug"] (NoArg (\ opts -> opts { debug = True })) "Enter debug mode"
    ]

parse :: [String] -> IO (Args, [String])
parse argv =
    case getOpt Permute options argv of
        (o,n,[]) -> do
            if "-h" `elem` argv
                then do 
                    hPutStrLn stderr (usageInfo header options)
                    exitSuccess
                else
                    return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: glados [...options] < file.scm"

