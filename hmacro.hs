import Expander
import Parser
import Control.Exception
import System.Environment
import GHC.IO.Handle
import System.IO

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



parseFile :: String -> Either [ErrorType] String
parseFile = (\filetext -> (parse filetext) >>= expand)

convertErrMsgs :: [ErrorType] -> String
convertErrMsgs errs = concat (map (\(lineno, colno, msg) -> "Line " ++ (show lineno) ++ ", Col " ++ (show colno) ++ ": " ++ msg ++ "\n") errs)

parseFiles :: CliArgs -> Either String (String, Maybe String)
parseFiles Help = Right ("Help message here", Nothing)
parseFiles Version = Right ("Version message here", Nothing)
parseFiles (Files (texts, out)) = do
        result <- (gatherEithers . (map (convertLeft (convertErrMsgs))) . (map (\a -> parse a >>= expand))) texts
        return (concat result, out)

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
main = (printException . writeResult) =<< (parseFiles =<<) <$> getCliArgs

