import Expander
import Parser
import Iohandling
import System.IO
import Debug.Trace

convertErrMsgs :: [ErrorType] -> String
convertErrMsgs errs = concat (map (\(lineno, colno, msg) -> "\x1b[1;31mError:\x1b[39m Line " ++ (show lineno) ++ ", Col " ++ (show colno) ++ ": " ++ msg ++ "\x1b[22m\n") errs)


parseToTxt :: String -> IO (Either [ErrorType] String)
parseToTxt s = expand `unwrapEither` parse s


parseAll :: [String] -> IO (Either String String)
parseAll strs = (convertErrMsgs `fmapLeft`) <$> (concat <$>) <$> (gatherEithers) <$> (mapM (parseToTxt) strs)

parseFiles :: CliArgs -> IO (Either String (String, Maybe String))
parseFiles Help = return (Right ("Help message here", Nothing))
parseFiles Version = return (Right ("Version message here", Nothing))
parseFiles (Files (filenames, outname)) = do
        filedat <- openFiles filenames
        parsed <- parseAll `unwrapEither` filedat
        return ((\a -> (a, outname)) <$> parsed)

openFiles :: [String] -> IO (Either String [String])
openFiles files = do
        either_filestrs <- stringifyException (mapM (readFile) files)
        return (fmapLeft (\a -> "\x1b[1;31mError: \x1b[39m" ++ a ++ "\x1b[22m\n") either_filestrs)

writeResult :: Either String (String, Maybe String) -> IO ()
writeResult (Left e) = hPutStr stderr e
writeResult (Right (txt, Just fname)) = writeFile fname txt
writeResult (Right (txt, Nothing)) = hPutStr stdout txt


main :: IO ()
main = writeResult =<< ((parseFiles `unwrapEither`) =<< getCliArgs)

