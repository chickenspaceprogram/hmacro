module Expander (expand, gatherEithers, fmapLeft) where

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

import Parser
import Iohandling
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Either
import Data.Char
import Debug.Trace

data MacroElement = Arg Int | Text String deriving (Eq, Show)
type MacroExpansion = [MacroElement]
type MacroMap = Map.Map String MacroExpansion

enumerate :: [a] -> [(Int, a)]
enumerate lst = zip (findIndices (const True) lst) lst

indexMap :: [a] -> Map.Map Int a
indexMap = Map.fromList . enumerate

fmapLeft :: (a -> c) -> Either a b -> Either c b
fmapLeft fn (Left e) = Left $ fn e
fmapLeft _ (Right v) = Right v

bindLeft :: (a -> Either b c) -> Either a c -> Either b c
bindLeft fn (Right v) = Right v
bindLeft fn (Left e) = fn e

apLeft :: Either (a -> b) c -> Either a c -> Either b c
apLeft fn eth = (`fmapLeft` eth) `bindLeft` fn

liftA2Left :: (a -> b -> c) -> Either a v -> Either b v -> Either c v
liftA2Left fn e1 e2 = fn `fmapLeft` e1 `apLeft` e2

expandElement :: Map.Map Int String -> MacroElement -> Either String String
expandElement _ (Text txt) = Right txt
expandElement mp (Arg arg) = toEither ("Macro argument `" ++ show arg  ++ "` not found.") (Map.lookup arg mp)

-- args -> how to expand -> result
expandMacro :: [String] -> MacroExpansion -> Either String String
expandMacro strs exp = let strmap = indexMap strs in
        concat `fmap` (mapM (expandElement strmap) exp)

toEither :: a -> Maybe b -> Either a b
toEither _ (Just v) = Right v
toEither e Nothing = Left e

gatherEithers :: [Either [a] b] -> Either [a] [b]
gatherEithers es = case lefts es of
        [] -> Right $ rights es
        ls -> Left $ concat ls

isIdStart :: Char -> Bool
isIdStart '_' = True
isIdStart '-' = True
isIdStart ch | isAsciiUpper ch || isAsciiLower ch = True
             | otherwise = False


data MacroExpansionToken = Numeric String | ArgNumeric Int | Ch Char

tokenize :: Char -> [MacroExpansionToken] -> [MacroExpansionToken]
tokenize ch ((Numeric num):rest) | isDigit ch = (Numeric (ch:num)):rest
                                          | ch == '$' = (ArgNumeric $ read num):rest
                                          | otherwise = (Ch ch):((map (Ch) (num)) ++ rest)
tokenize ch rest | isDigit ch = (Numeric [ch]):rest
tokenize '\\' ((ArgNumeric num):rest) = (map (Ch) (show num)) ++ rest
tokenize ch rest = (Ch ch):rest

convert :: MacroExpansionToken -> MacroElement
convert (Numeric num) = Text (show num)
convert (ArgNumeric num) = Arg num
convert (Ch ch) = Text [ch]

isText :: MacroElement -> MacroElement -> Bool
isText (Text _) (Text _) = True
isText _ _ = False

unwrapText :: MacroElement -> String
unwrapText (Text txt) = txt

concatText :: [MacroElement] -> MacroElement
concatText all@((Text txt):rest) = Text (concat (map (unwrapText) all))
concatText [e] = e

processExpansion :: String -> MacroExpansion
processExpansion s = (concatText) `map` (groupBy (isText) ((convert) `map` (foldr (tokenize) [] s)))

parseDefArgs :: (Int, Int) -> MacroMap -> [Element] -> Either [ErrorType] (String, MacroExpansion)
parseDefArgs (lineno, colno) mmap ((_, _, Elements name):(_, _, Elements expansion):[]) = do
        expandedName <- expandInternal mmap name
        expandedElem <- expandInternal mmap expansion
        case expandedName of
                (x:_) -> if (isIdStart x) && all (\c -> isIdStart c || isDigit c) expandedName && expandedName /= "def" && expandedName /= "include"
                         then Right (expandedName, processExpansion expandedElem)
                         else Left [(lineno, colno, "Macro name `" ++ expandedName ++ "`) contains invalid characters or is the name of an inbuilt macro.")]
                _ -> Left [(lineno, colno, "Cannot define a macro without a name.")]

parseDefArgs (row, col) mmap elems = Left [(row, col, "`def` macro called with an invalid number of arguments: `def` expects 2 arguments, provided " ++ show (length elems) ++ ".")]

-- this code needs a refactor to have two different expander functions
-- one that only expands macros
-- and one that converts nonmacros to text
-- but imma be honest with you that is not happening anytime soon lmao
expandInternal :: MacroMap -> [Element] -> Either [ErrorType] String
expandInternal mmap ((_, _, Plaintext str):xs) = do
        result <- expandInternal mmap xs
        Right (str ++ result)
expandInternal mmap ((_, _, Elements els):xs) = expandInternal mmap els
expandInternal mmap ((line, col, Macro (name, args)):xs) | name == "def" = do
        (newname, newexp) <- parseDefArgs (line, col) mmap args
        expandInternal (Map.insert newname newexp mmap) xs
                                                         | otherwise = do
        expansionRules <- ((toEither [(line, col, "Macro name `" ++ name ++ "` undefined.")]) . Map.lookup name ) mmap
        expandedArgs <- (gatherEithers . (map (\(_, _, Elements els) -> expandInternal mmap els))) args
        result <- fmapLeft (\str -> [(line, col, str)]) (expandMacro expandedArgs expansionRules)
        rest <- expandInternal mmap xs
        return (result ++ rest)
expandInternal _ [] = Right []

includeFiles :: Set.Set String -> MacroMap -> [Element] -> IO (Either [ErrorType] [Element])
includeFiles fnames mmap (el@(_, _, Plaintext str):xs) = do 
        fils <- includeFiles fnames mmap xs
        return $ fmap (el :) fils
includeFiles fnames mmap ((lineno, colno, Elements els):xs) = do
        children <- includeFiles fnames mmap els
        siblings <- includeFiles fnames mmap xs
        return $ catEithers (fmap (\a -> (lineno, colno, Elements a)) children) siblings
includeFiles fnames mmap ((lineno, colno, Macro (macroname, (_, _, Elements filname):_)):xs) | macroname == "include" = do
        argincl <- includeFiles fnames mmap filname
        let filname' = (argincl >>= (expandInternal mmap)) >>= (\nm -> if Set.member nm fnames then Left [(lineno, colno, "The file " ++ nm ++ " has been included previously.")] else Right nm)
        fildat <- unwrapEither (readFileExcept lineno colno) filname'
        let elems = (fildat >>= parse)
        case filname' of
                Right name -> unwrapEither (\e -> includeFiles (Set.insert name fnames) mmap (e ++ xs)) elems
                Left e -> return $ Left e

includeFiles fnames mmap ((lineno, colno, Macro (macroname, [])):xs) | macroname == "include" = return $ Left [(lineno, colno, "\\include macro has no arguments: must provide a file to include.")]
includeFiles fnames mmap ((lineno, colno, Macro (macroname, args)):xs) | macroname == "def" = do
        valid <- includeFiles fnames mmap args
        case parseDefArgs (lineno, colno) mmap args of
                Left err -> return $ Left err
                Right (name, exp) -> (includeFiles fnames (Map.insert name exp mmap) xs)

        
includeFiles fnames mmap ((lineno, colno, Macro (nm, args)):xs) = do
        argerr <- includeFiles fnames mmap args
        rest <- includeFiles fnames mmap xs
        let ptres = (\txt -> (lineno, colno, Plaintext txt)) <$> ((expandInternal mmap) =<< ((\arg -> [(lineno, colno, Macro (nm, args))]) <$> argerr))
        return $ catEithers ptres rest

includeFiles _ _ [] = return $ Right []

catEithers :: Either [e] v -> Either [e] [v] -> Either [e] [v]
catEithers (Right val) (Right vals) = Right $ val:vals
catEithers (Left e) (Left es) = Left $ e ++ es
catEithers (Left e) _ = Left e
catEithers _ (Left es) = Left es

readFileExcept :: Int -> Int -> String -> IO (Either [ErrorType] String)
readFileExcept lineno colno filename = do
        result <- (stringifyException $ readFile filename)
        return ((\a -> [(lineno, colno, "Error reading file in \\include: " ++ a)]) `fmapLeft` result)

expand :: [Element] -> IO (Either [ErrorType] String)
expand els = do
        inclres <- includeFiles Set.empty Map.empty els
        return $ inclres >>= (expandInternal Map.empty)
