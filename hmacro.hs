import Expander
import Parser
import Control.Exception
import System.Environment
import GHC.IO.Handle
import System.IO
import Debug.Trace

data CliArgs = Help | Version | Files ([String], Maybe String)

-- ([input files, cat these], output filenmae)
parseCliArgs :: [String] -> Either String CliArgs
parseCliArgs (x:xs) 
                | x == "-o" || x == "--output" = do
                rest <- parseCliArgs xs
                case rest of
                        Help -> Right Help
                        Version -> Right Version
                        Files (outfile:args, Nothing) -> Right (Files (args, Just outfile))
                        Files ([], _) -> Left "No output filename provided."
                        Files (_, Just str) -> Left ("Output file `" ++ str ++ "`already specified, cannot output to multiple files.")

                | x == "-v" || x == "--version" = Right Version
                | x == "--help" = Right Help
                | otherwise = do
                rest <- parseCliArgs xs
                case rest of
                        Help -> Right Help
                        Version -> Right Version
                        Files (fil, out) -> Right (Files (x:fil, out))
parseCliArgs [] = Right (Files ([], Nothing))

stringifyException :: forall a. IO a -> IO (Either String a)
stringifyException io = do
        e <- (try io) :: IO (Either IOException a)
        case e of
                Left err -> (return . Left . show) err
                Right v -> return (Right v)

printException :: IO () -> IO ()
printException monad = (\a -> case a of Right _ -> return (); Left e -> hPutStr stderr e) =<< stringifyException monad

getCliArgs :: IO (Either String CliArgs)
getCliArgs = (parseCliArgs =<<) <$> (stringifyException (getArgs)) -- evil

convertErrMsgs :: [ErrorType] -> String
convertErrMsgs errs = concat (map (\(lineno, colno, msg) -> "\x1b[1;31mError:\x1b[39m Line " ++ (show lineno) ++ ", Col " ++ (show colno) ++ ": " ++ msg ++ "\x1b[22m\n") errs)

-- in combo with =<< this can double-unwrap a monad containing an either
-- evil
unwrapEither :: Monad m => (a -> m (Either e b)) -> Either e a -> m (Either e b)
unwrapEither fn (Right val) = fn val
unwrapEither _ (Left e) = return (Left e)

parseFiles :: CliArgs -> IO (Either String (String, Maybe String))
parseFiles Help = return (Right ("Help message here", Nothing))
parseFiles Version = return (Right ("Version message here", Nothing))
parseFiles all@(Files _) = do
        Files ((opentexts, outfile)) <- openFiles all
        return ((\result -> Right (concat result, outfile)) =<< (gatherEithers . (map (convertLeft (convertErrMsgs))) . (map (\a -> (parse a) >>= expand))) opentexts)

-- will never actually give an [ErrorType], this is a hack
openFiles :: CliArgs -> IO CliArgs
openFiles Help = return Help
openFiles Version = return Version
openFiles (Files (names, out)) = do 
        filedata <- mapM (readFile) names
        return (Files (filedata, out))

writeResult :: Either String (String, Maybe String) -> IO ()
writeResult (Left e) = hPutStr stderr e
writeResult (Right (txt, Just fname)) = writeFile fname txt
writeResult (Right (txt, Nothing)) = hPutStr stdout txt


main :: IO ()
main = ((printException . writeResult) =<< (unwrapEither parseFiles =<<) getCliArgs)

