import qualified Data.Map.Strict as Map
import Data.Char
import Data.List
import Text.Show.Functions
import Debug.Trace
main = do print "asdf"

data Token = Chr Char | NameStart | ScopeStart | ScopeEnd deriving Eq
data Gathered = Text String | Name String | LexError String | StartScope | EndScope deriving Eq
data ScopeElem = ScopeText String | MacroName String | Scope Scope | ParseError String deriving Eq
type Scope = [ScopeElem]

tokenize :: String -> [Token]
tokenize ('\\':'\\':xs) = (Chr '\\'):(tokenize xs)
tokenize ('\\':'{':xs) = (Chr '{'):(tokenize xs)
tokenize ('\\':'}':xs) = (Chr '}'):(tokenize xs)
tokenize ('\\':xs) = NameStart:(tokenize xs)
tokenize ('{':xs) = ScopeStart:(tokenize xs)
tokenize ('}':xs) = ScopeEnd:(tokenize xs)
tokenize (x:xs) = (Chr x):(tokenize xs)
tokenize [] = []

isIdChar :: Token -> Bool
isIdChar (Chr '_') = True
isIdChar (Chr '-') = True
isIdChar (Chr x) | isDigit x || isAsciiUpper x || isAsciiLower x = True
isIdChar _ = False

isChr :: Token -> Bool
isChr (Chr _) = True
isChr _ = False

gather :: [Token] -> [Gathered]
gather (ScopeStart:xs) = StartScope:(gather xs)
gather (ScopeEnd:xs) = EndScope:(gather xs)
gather (NameStart:Chr x:xs) | x == '_' || x == '-' || isAsciiUpper x || isAsciiLower x = 
        let (tail, head) = span (isIdChar) xs in
                ((Name (fmap (\(Chr c) -> c) head)):(gather tail))
gather all@(Chr x:xs) =
        let (tail, head) = span (isChr) all in
                ((Text (fmap (\(Chr c) -> c) head)):(gather tail))
gather (x:xs) = (LexError "Bad character in macro usage"):(gather xs)
gather [] = []

scopeInternal :: [Gathered] -> (Scope, [Gathered])
scopeInternal ((Name name):xs) = let (rest, tail) = scopeInternal xs in
        ((MacroName name):rest, tail)
scopeInternal ((Text txt):xs) = let (rest, tail) = scopeInternal xs in
        ((ScopeText txt):rest, tail)
scopeInternal (StartScope:xs) = case scopeInternal xs of
        (children, []) -> ((ParseError "No closing brace"):children, []) -- pretend there was no opening brace and continue parsing
        (children, tail) -> let (rest, tail') = scopeInternal tail in
                                ((Scope children):rest, tail')
scopeInternal (EndScope:xs) = ([], xs)
scopeInternal ((LexError e):xs) = let (rest, tail) = scopeInternal xs in
        ((ParseError e):rest, tail) -- bit of a hack
scopeInternal [] = ([], [])

scope :: [Gathered] -> Scope
scope s = case scopeInternal s of
               (scp, []) -> scp
               (scp, txt) -> scp ++ [ParseError "Extraneous closing brace"] ++ (scope txt)

incrementPos :: (Int, Int) -> String -> (Int, Int)
incrementPos (lineno, colno) str = case lines str of
                                       [x] -> (lineno, colno + (length x))
                                       splitstr -> (lineno + (length splitstr) - 1, (length . last) splitstr)

validateScopeInternal :: (Int, Int) -> Scope -> ([(Int, Int, String)], (Int, Int))
validateScopeInternal (lineno, colno) (ParseError e:xs) = let (errs, pos') = validateScopeInternal (lineno, colno) xs in
        (((lineno, colno, e):errs), pos')
validateScopeInternal pos (Scope s:xs) = let (childerrs, pos') = validateScopeInternal pos s
                                             (sameerrs, pos'') = validateScopeInternal pos' xs in
                                             (childerrs ++ sameerrs, pos'')
validateScopeInternal pos@(lineno, colno) (MacroName name:xs) = validateScopeInternal (lineno, colno + 1 + (length name)) xs
validateScopeInternal pos@(lineno, colno) (ScopeText name:xs) = validateScopeInternal (incrementPos pos name) xs
validateScopeInternal pos _ = ([], pos)

validateScope :: Scope -> Either [(Int, Int, String)] Scope
validateScope scp = case validateScopeInternal (1, 1) scp of
                         ([], _) -> Right scp
                         (errs, _) -> Left errs

