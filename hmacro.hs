import qualified Data.Map.Strict as Map
import Data.Char
import Text.Show.Functions
import Debug.Trace
main = do print "asdf"



data MacroElement = 
        Text String
        | Arg Int deriving Show

type Parsed = String
type UnParsed = String

parseText :: (String) -> (Map.Map String [MacroElement]) -> Either String (Parsed, UnParsed)
parseText ('\\':'\\':rest) (fnmap) = do
        (parsed, unparsed) <- parseText rest fnmap
        Right ('\\':parsed, unparsed)
parseText ('\\':'{':rest) (fnmap) = do
        (parsed, unparsed) <- parseText rest fnmap
        Right ('{':parsed, unparsed)
parseText ('\\':'}':rest) (fnmap) = do
        (parsed, unparsed) <- parseText rest fnmap
        Right ('}':parsed, unparsed)
parseText ('}':rest) (fnmap) = Right ("", '}':rest)
parseText ('\\':xs) (fnmap) = do
        (expanded_text, rest) <- parseMacro xs fnmap
        (parsed, unparsed) <- parseText rest fnmap
        Right (expanded_text ++ parsed, unparsed)
parseText (x:xs) (fnmap) = do
        (parsed, unparsed) <- parseText xs fnmap
        Right(x:parsed, unparsed)
parseText [] _ = Right ([], [])

parseMacro :: String -> (Map.Map String [MacroElement]) -> Either String (String, String)
parseMacro str@(x:_) (map) | isAsciiUpper x || isAsciiLower x || x == '-' || x == '_' = let (name, args_txt) = getName str in do
        fn <- checkName name map
        (args, rest) <- getArgs 1 args_txt map
        expanded_text <- expandMacro fn args
        Right (expanded_text, rest)
parseMacro _ _ = Left ("Invalid macro name.")

getName :: String -> (String, String)
getName (x:xs) | isDigit x || isAsciiUpper x || isAsciiLower x || x == '-' || x == '_' =
        (\(name, rest) -> ((x:name), rest)) (getName xs)
getName (x:xs) = ([], x:xs)
getName [] = ([], [])

checkName :: String -> (Map.Map String [MacroElement]) -> Either String [MacroElement]
checkName name map = 
        case (Map.lookup name map) of
                Just fn -> Right fn
                Nothing -> Left ("Parse error: Macro `" ++ name ++ "` not defined.")

-- start arg number -> unparsed txt -> identifier map -> Either err (args_map, rest)
getArgs :: Int -> String -> (Map.Map String [MacroElement]) -> Either String (Map.Map Int String, String)
getArgs current_num ('{':xs) (id_map) = do
        (parsed, unparsed) <- parseText xs id_map
        case unparsed of
                ('}':rest) -> do
                        (return_map, return_tail) <- getArgs (current_num + 1) rest id_map
                        Right (Map.insert current_num parsed return_map, return_tail)
                [] -> Left ("Failed to find ending brace when expanding a macro argument.")
                _ -> Left ("An unknown error occurred, maybe you forgot to include a closing brace on a macro?")
getArgs _ xs _ = Right (Map.empty, xs)

expandMacro :: [MacroElement] -> (Map.Map Int String) -> Either String String
expandMacro [] _ = Right []
expandMacro ((Text txt):xs) arg_values = do
        result <- expandMacro xs arg_values
        Right (txt ++ result)
expandMacro ((Arg arg):xs) arg_values = do
        arg_txt <- case (Map.lookup arg arg_values) of
                Just txt -> Right txt
                Nothing -> Left ("Argument `" ++ show arg ++ "` was not provided in macro invocation.")
        rest <- expandMacro xs arg_values
        Right (arg_txt ++ rest)

