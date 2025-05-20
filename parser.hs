module Parser (Element, parse, ElementData (Plaintext, Elements, Macro), Macro, ErrorType) where

-- hmacro - a macro-expander written in Haskell
-- Copyright (C) 2025 Athena Boose
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Check the file `LICENSE` for a copy of the license.

import Data.Char
import Data.List
import Debug.Trace

data Token = Chr Char | NameStart | ScopeStart | ScopeEnd deriving (Eq, Show)
data Gathered = GatheredText String | Name String | LexError String | StartScope | EndScope deriving (Eq, Show)
data ScopeElem = ScopeText String | MacroName String | Scope Scope | ParseError String deriving (Eq, Show)
type Scope = [ScopeElem]
data TaggedScopeElem = TaggedScopeText String | TaggedMacroName String | TaggedScope TaggedScope | TaggedParseError String deriving (Eq, Show)
type TaggedScope = [(Int, Int, TaggedScopeElem)]

type ErrorType = (Int, Int, String) -- line, col, msg
type Element = (Int, Int, ElementData)
data ElementData = Plaintext String | Elements [Element] | Macro Macro deriving (Eq, Show)
type Macro = (String, [Element])

parse :: String -> Either [ErrorType] [Element]
parse str = groupMacros <$> (validateScope . tagNodes . scope . gather . tokenize) str

-- name, list of args

tokenizeFolder :: Char -> [Token] -> [Token]
tokenizeFolder '\\' (NameStart:xs) = (Chr '\\'):xs
tokenizeFolder '\\' (ScopeStart:xs) = (Chr '{'):xs
tokenizeFolder '\\' (ScopeEnd:xs) = (Chr '}'):xs
tokenizeFolder '\\' (Chr '$':xs) = (Chr '\\'):(Chr '$'):xs
tokenizeFolder '\\' (Chr '\n':xs) = xs
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

numNewlines :: String -> Int
numNewlines = length . (filter ('\n' ==))

incLineNo :: Char -> (Bool, Int) -> (Bool, Int)
incLineNo ch (True, num) | ch == '\n' = (False, num)
                         | otherwise = (True, num + 1)
incLineNo _ v = v

incrementPos :: (Int, Int) -> String -> (Int, Int)
incrementPos (lineno, colno) str = let (noNewLine, len) = lenLastLine str in
                                       (lineno + (numNewlines str), len + if noNewLine then colno else 1)


lenLastLine :: String -> (Bool, Int)
lenLastLine = foldr (incLineNo) (True, 0)

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
convertScope (i, j, (TaggedScopeText txt)) = (i, j, Plaintext txt)

groupFolder :: TaggedScope -> (Int, Int, TaggedScopeElem) -> (TaggedScope, [Element])
groupFolder scp elem@(lineno, colno, TaggedScopeText txt) = ([], (convertScope elem):(map (convertScope) scp)) -- flush scope stack
groupFolder scp newscp@(lineno, colno, TaggedScope _) = (newscp:scp, []) -- push onto scope stack
groupFolder scp (lineno, colno, TaggedMacroName nm) = ([], [(lineno, colno, Macro (nm, map (convertScope) scp))]) -- flush scope stack into a macro

groupMacros :: TaggedScope -> [Element]
groupMacros ts = let (scp, elems) = mapAccumR (groupFolder) [] ts in
        (map (convertScope) scp) ++ (concat elems) -- flush any trailing scope stack



