module Iohandling (unwrapEither, CliArgs (Help, Version, License, Files), getCliArgs, stringifyException) where
import Control.Exception
import System.Environment
import Debug.Trace

data CliArgs = Help | Version | License | Files ([String], Maybe String)

-- in combo with =<< this can double-unwrap a monad containing an either
-- evil
unwrapEither :: Monad m => (a -> m (Either e b)) -> Either e a -> m (Either e b)
unwrapEither fn (Right val) = fn val
unwrapEither _ (Left e) = return (Left e)

getCliArgs :: IO (Either String CliArgs)
getCliArgs = (parseCliArgs =<<) <$> (stringifyException (getArgs)) -- evil

-- ([input files], output filenmae)
parseCliArgs :: [String] -> Either String CliArgs
parseCliArgs (x:xs) 
                | x == "-o" || x == "--output" = do
                rest <- parseCliArgs xs
                case rest of
                        Help -> Right Help
                        Version -> Right Version
                        License -> Right License
                        Files (outfile:args, Nothing) -> Right (Files (args, Just outfile))
                        Files ([], _) -> Left "No output filename provided."
                        Files (_, Just str) -> Left ("Output file `" ++ str ++ "`already specified, cannot output to multiple files.")

                | x == "-v" || x == "--version" = Right Version
                | x == "--help" = Right Help
                | x == "--license" = Right License
                | otherwise = do
                rest <- parseCliArgs xs
                case rest of
                        Help -> Right Help
                        Version -> Right Version
                        License -> Right License
                        Files (fil, out) -> Right (Files (x:fil, out))
parseCliArgs [] = Right (Files ([], Nothing))

stringifyException :: forall a. IO a -> IO (Either String a)
stringifyException io = do
        e <- (try io) :: IO (Either IOException a)
        case e of
                Left err -> (return . Left . show) err
                Right v -> return (Right v)

