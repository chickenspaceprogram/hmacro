module Parsing (Element, parse, ElementData (Plaintext, Elements, Macro), Macro) where

import Data.Char
import Data.List

data Token = Chr Char | NameStart | ScopeStart | ScopeEnd deriving (Eq, Show)
data Gathered = GatheredText String | Name String | LexError String | StartScope | EndScope deriving (Eq, Show)
data ScopeElem = ScopeText String | MacroName String | Scope Scope | ParseError String deriving (Eq, Show)
type Scope = [ScopeElem]
data TaggedScopeElem = TaggedScopeText String | TaggedMacroName String | TaggedScope TaggedScope | TaggedParseError String deriving (Eq, Show)
type TaggedScope = [(Int, Int, TaggedScopeElem)]
type ErrorType = (Int, Int, String) -- line, col, msg


type Element = (Int, Int, ElementData)
data ElementData = Plaintext String | Elements [Element] | Macro Macro
type Macro = (String, [Element])

parse :: String -> Either [ErrorType] [Element]
parse str = groupMacros `fmap` (validateScope . tagNodes . scope . gather . tokenize) str

-- name, list of args

tokenizeFolder :: Char -> [Token] -> [Token]
tokenizeFolder '\\' (NameStart:xs) = (Chr '\\'):xs
tokenizeFolder '\\' (ScopeStart:xs) = (Chr '{'):xs
tokenizeFolder '\\' (ScopeEnd:xs) = (Chr '}'):xs
tokenizeFolder '\\' xs = (NameStart):xs
tokenizeFolder '{' xs = (ScopeStart):xs
tokenizeFolder '}' xs = (ScopeEnd):xs
tokenizeFolder ch xs = (Chr ch):xs

tokenize :: String -> [Token]
tokenize = foldr tokenizeFolder []

tokenize' :: String -> [Token]
tokenize' ('\\':'\\':xs) = (Chr '\\'):(tokenize' xs)
tokenize' ('\\':'{':xs) = (Chr '{'):(tokenize' xs)
tokenize' ('\\':'}':xs) = (Chr '}'):(tokenize' xs)
tokenize' ('\\':xs) = NameStart:(tokenize' xs)
tokenize' ('{':xs) = ScopeStart:(tokenize' xs)
tokenize' ('}':xs) = ScopeEnd:(tokenize' xs)


gatherFolder :: Token -> [Gathered] -> [Gathered]
gatherFolder ScopeStart acc = StartScope:acc
gatherFolder ScopeEnd acc = EndScope:acc
gatherFolder (Chr c) (GatheredText x:xs) = (GatheredText (c:x)):xs
gatherFolder (Chr c) acc = (GatheredText [c]):acc
gatherFolder NameStart acc@(GatheredText x:xs) = 
        let (head, tail) = span (\x -> x == '_' || x == '-' || isDigit x || isAsciiUpper x || isAsciiLower x) x in
                case (head, tail) of
                        ([], _) -> (LexError "Bad character in macro usage"):acc
                        (h, []) -> (Name h):xs
                        (h, t) -> (Name head):(GatheredText t):xs
gatherFolder NameStart acc = (LexError "Bad character in macro usage"):acc

gather :: [Token] -> [Gathered]
gather = foldr gatherFolder []

scopeFolder :: ([[Gathered]], Scope) -> Gathered -> ([[Gathered]], Scope)
scopeFolder ([], scp) (Name name) = ([], (MacroName name):scp)
scopeFolder ([], scp) (GatheredText txt) = ([], (ScopeText txt):scp)
scopeFolder (x:xs, scp) (Name name) = (((Name name):x):xs, scp)
scopeFolder (x:xs, scp) (GatheredText name) = (((GatheredText name):x):xs, scp)

scopeInternal :: [Gathered] -> (Scope, [Gathered])
scopeInternal ((Name name):xs) = let (rest, tail) = scopeInternal xs in
        ((MacroName name):rest, tail)
scopeInternal ((GatheredText txt):xs) = let (rest, tail) = scopeInternal xs in
        ((ScopeText txt):rest, tail)
scopeInternal (StartScope:xs) = case scopeInternal xs of
        (children, EndScope:tail) -> let (rest, tail') = scopeInternal tail in
                                ((Scope children):rest, tail')
        (children, tail) -> let (rest, tail') = scopeInternal tail in  -- could just have tail be [] but this handles all cases guaranteed
                                ((ParseError "No closing brace"):rest, tail') -- pretend there was no opening brace and continue parsing
scopeInternal all@(EndScope:xs) = ([], all)
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

tagNodesInternal :: (Int, Int) -> Scope -> ((Int, Int), TaggedScope)
tagNodesInternal pos@(lineno, colno) (ScopeText txt:xs) = let (pos', rest) = tagNodesInternal (incrementPos pos txt) xs in
        (pos', (lineno, colno, TaggedScopeText txt):rest)
tagNodesInternal pos@(lineno, colno) (MacroName name:xs) = let (pos', rest) = tagNodesInternal (lineno, colno + 1 + (length name)) xs in 
        (pos', (lineno, colno, TaggedMacroName name):rest)
tagNodesInternal pos@(lineno, colno) (Scope scp:xs) = let (pos', children) = tagNodesInternal (lineno, colno + 1) scp
                                                          (pos'', siblings) = tagNodesInternal pos' xs in
        (pos'', (lineno, colno, TaggedScope children):siblings)
tagNodesInternal pos@(lineno, colno) (ParseError e:xs) = let (pos', rest) = tagNodesInternal pos xs in
        (pos', (lineno, colno, TaggedParseError e):rest)
tagNodesInternal pos [] = (pos, [])

tagNodes :: Scope -> TaggedScope
tagNodes scp = let (_, result) = tagNodesInternal (1, 1) scp in result


filterErrors :: TaggedScope -> [ErrorType]
filterErrors ((_, _, TaggedScope scp):xs) = (filterErrors scp) ++ (filterErrors xs)
filterErrors ((lineno, colno, TaggedParseError e):xs) = (lineno, colno, e):(filterErrors xs)
filterErrors (_:xs) = filterErrors xs
filterErrors [] = []

validateScope :: TaggedScope -> Either [ErrorType] TaggedScope
validateScope s = case filterErrors s of
                       [] -> Right s
                       any -> Left any

isScope :: (Int, Int, TaggedScopeElem) -> Bool
isScope (_, _, TaggedScope _) = True
isScope _ = False

convertScope :: (Int, Int, TaggedScopeElem) -> Element
convertScope (i, j, (TaggedScope s)) = (i, j, (Elements . groupMacros) s)

groupFolder :: TaggedScope -> (Int, Int, TaggedScopeElem) -> (TaggedScope, [Element])
groupFolder scp (lineno, colno, TaggedScopeText txt) = ([], map (convertScope) scp) -- flush scope stack
groupFolder scp newscp@(lineno, colno, TaggedScope _) = (newscp:scp, []) -- push onto scope stack
groupFolder scp (lineno, colno, TaggedMacroName nm) = ([], [(lineno, colno, Macro (nm, map (convertScope) scp))]) -- flush scope stack into a macro

groupMacros :: TaggedScope -> [Element]
groupMacros ts = let (scp, elems) = mapAccumR (groupFolder) [] ts in
        (map (convertScope) scp) ++ (concat elems) -- flush any trailing scope stack



