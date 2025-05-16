import qualified Data.Map.Strict as Map
import Data.Char
import Text.Show.Functions
import Debug.Trace
import Control.Monad.State
main = do print "asdf"


type Parsed = String
type UnParsed = String

data ExpandableMacroElement = 
        Text String
        | Arg Int deriving Show
type ExpandableMacro = [ExpandableMacroElement]
type ExpandableMacroMap = Map.Map String ExpandableMacro

type ParserStateType = (ExpandableMacroMap, UnParsed)
type ParserState = StateT ParserStateType (Either String)

type ArgsMap = Map.Map Int String

data ControlMacro =
        Def
        | Undef
        | Ifdef
        | Include

data AnyMacro = Ctrl ControlMacro | Expand ExpandableMacro

convertMaybe :: String -> Maybe a -> Either String a
convertMaybe _ (Just a) = Right a
convertMaybe str Nothing = Left str

control_macros = Map.fromList [("def", Def), ("undef", Undef), ("ifdef", Ifdef), ("include", Include)]


parseText :: ParserStateType -> Either String (Parsed, ParserStateType)
parseText ('\\':'\\':rest, state) = do
        (rest', state') <- parseText rest state
        return ('\\':rest', state')
parseText ('\\':'{':rest, state) = do 
        (rest', state') <- parseText rest state
        return ('{':rest', state')
parseText ('\\':'}':rest, state) = do
        (rest', state') <- parseText rest state
        return ('}':rest', state')
parseText all@('}':rest, fnmap) = return ("", all)
parseText ('\\':xs, fnmap) = do
        (expanded_text, rest, newmap) <- parseMacro xs fnmap
        (parsed, unparsed, newermap) <- parseText rest newmap
        return (expanded_text ++ parsed, unparsed, newermap)
parseText (x:xs, fnmap) = do
        (parsed, unparsed, newmap) <- parseText xs fnmap
        return(x:parsed, unparsed, newmap)
parseText ([], map) = return ([], [], map)

parseMacro :: String -> ExpandableMacroMap -> Either String (String, String, ExpandableMacroMap)
parseMacro str@(x:_) (map) | isAsciiUpper x || isAsciiLower x || x == '-' || x == '_' = let (name, args_txt) = getName str in do
        fn <- checkName name map
        (args, rest, newmap) <- getArgs 1 args_txt map
        expanded_text <- expandMacro fn args
        return (expanded_text, rest, newmap)
parseMacro _ _ = returnError ("Invalid macro name.")

getName :: ParserStateType -> Either String (String, ParserStateType)
getName all@(_, (x:xs)) | isDigit x || isAsciiUpper x || isAsciiLower x || x == '-' || x == '_' = do
        (rest, state) <- getName all
        Right (x:rest, state)
getName any = Right ([], any)

checkName :: String -> ParserStateType -> Either String (AnyMacro, ParserStateType)
checkName name st | Map.member name control_macros = do
        enum <- convertMaybe "this should never happen" Map.lookup name control_macros
        Right (Ctrl enum, st)
checkName name st@(map, _) = do
        fn <- convertMaybe ("Invalid macro name `" ++ name ++ "`.") (Map.lookup name map)
        Right (Expand fn, st)

-- start arg number -> unparsed txt -> identifier map -> Either err (args_map, rest, newmap)
getArgs :: Int -> ParserStateType -> Either String (ArgsMap, ParserStateType)
getArgs current_num ('{':xs, map) = do
        (parsed, unparsed, map') <- parseText xs map
        case unparsed of
                ('}':rest) -> do
                        (return_map, return_tail, finalnewmap) <- getArgs (current_num + 1) rest newmap
                        return (Map.insert current_num parsed return_map, return_tail, finalnewmap)
                [] -> returnError ("Failed to find ending brace when expanding a macro argument.")
                _ -> returnError ("An unknown error occurred, maybe you forgot to include a closing brace on a macro?")
getArgs _ state = return (Map.empty, state)

expandMacro :: ExpandableMacro -> ArgsMap -> Either String String
expandMacro [] _ = return []
expandMacro ((Text txt):rest) arg_values = do
        result <- expandMacro rest arg_values
        return (txt ++ result)
expandMacro ((Arg arg):rest) arg_values = do
        arg_txt <- case (Map.lookup arg arg_values) of
                Just txt -> return txt
                Nothing -> returnError ("Argument `" ++ show arg ++ "` was not provided in macro invocation.")
        rest <- expandMacro rest arg_values
        return (arg_txt ++ rest)

controlMacro :: ControlMacro -> ArgsMap -> ExpandableMacroMap -> Either String (String, ExpandableMacroMap)
controlMacro Def args map = do
        name <- case Map.lookup 1 args of
                Just txt -> return txt
                Nothing -> returnError ("Did not provide an argument to \\def macro.")
        returnError name -- not done yeet
