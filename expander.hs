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

fmapLeft :: (a -> c) -> Either a b -> Either c b
fmapLeft fn (Left e) = Left (fn e)
fmapLeft fn (Right v) = Right (v)

gatherEithers :: [Either [a] b] -> Either [a] [b]
gatherEithers es = case lefts es of
        [] -> Right (rights es)
        ls -> Left (concat ls)

isIdStart :: Char -> Bool
isIdStart '_' = True
isIdStart '-' = True
isIdStart ch | isAsciiUpper ch || isAsciiLower ch = True
             | otherwise = False


data MacroExpansionToken = Numeric Int | ArgNumeric Int | Ch Char

tokenize :: Char -> [MacroExpansionToken] -> [MacroExpansionToken]
tokenize ch ((Numeric num):rest) | isDigit ch = (Numeric (num * 10 + (digitToInt ch))):rest
                                          | ch == '$' = (ArgNumeric num):rest
                                          | otherwise = (Ch ch):((map (Ch) (show num)) ++ rest)
tokenize ch rest | isDigit ch = (Numeric (digitToInt ch)):rest
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

parseDefArgs :: (Int, Int) -> MacroMap -> [Element] -> IO (Either [ErrorType] (String, MacroExpansion))
parseDefArgs (lineno, colno) mmap ((_, _, Elements name):(_, _, Elements expansion):[]) = do
        expandedName <- expandInternal mmap name
        expandedElem <- expandInternal mmap expansion
        case (expandedName, expandedElem) of
                (Right nm@(x:_), Right el) -> if (isIdStart x) && all (\c -> isIdStart c || isDigit c) nm
                         then return (Right (nm, processExpansion el))
                         else return (Left [(lineno, colno, "Macro name `" ++ nm ++ "`) contains invalid characters.")])
                (Left errs, Right _) -> return (Left ((lineno, colno, "Encountered an error when expanding macro name."):errs))
                (Right _, Left errs) -> return (Left ((lineno, colno, "Encountered an error when expanding macro definition.j"):errs))
                (Left nameerrs, Left deferrs) -> return (Left ((lineno, colno, "Encountered an error when expanding macro name."):nameerrs ++ (lineno, colno, "Encountered an error when expanding macro definition.j"):deferrs))
                _ -> return (Left [(lineno, colno, "Cannot define a macro without a name.")])

parseDefArgs (row, col) mmap elems = return (Left [(row, col, "`def` macro called with an invalid number of arguments: `def` expects 2 arguments, provided " ++ show (length elems) ++ ".")])

-- position -> args
includeFile :: (Int, Int) -> [String] -> IO (Either [ErrorType] [Element])
includeFile (lineno, colno) [name:_] = return (Left [])
includeFile (lineno, colno) _ = return (Left [(lineno, colno, "No filename argument provided for \\include.")])

expandInternal :: MacroMap -> [Element] -> IO (Either [ErrorType] String)
expandInternal mmap ((_, _, Plaintext str):xs) = do
        result <- expandInternal mmap xs
        return ((str ++) <$> result)
expandInternal mmap ((_, _, Elements els):xs) = expandInternal mmap els
expandInternal mmap ((line, col, Macro (name, args)):xs) | name == "def" = do
        args <- parseDefArgs (line, col) mmap args
        (\(newname, newexp) -> expandInternal (Map.insert newname newexp mmap) xs) `unwrapEither` args
                                                         | otherwise = do
        expresults <- mapM (\(_, _, Elements e) -> expandInternal mmap e) args
        rest <- expandInternal mmap xs
        case (rest, Map.lookup name mmap) of
                (Right txt, Just rules) -> (\a -> a ++ txt) <$> ((\el -> fmapLeft (\msg -> (line, col, msg)) (expandElement el rules)) =<< (gatherEithers expresults))
                (Left errs, Nothing) -> return (Left((line, col, "Macro name `" ++ name ++ "` undefined."):errs))
                (Left errs, _) -> return (Left(errs))
                (_, Nothing) -> return (Left ([(line, col, "Macro name `" ++ name ++ "` undefined.")]))
--        gatherEithers expresults >>= (\e -> fmapLeft (\msg -> (line, col, msg)) (expandElement e -- macro element here))
--        expansionRules <- ((toEither [(line, col, "Macro name `" ++ name ++ "` undefined.")]) . Map.lookup name ) mmap
--        expandedArgs <- (gatherEithers . (map (\(_, _, Elements els) -> expandInternal mmap els))) args
--        result <- fmapLeft (\str -> [(line, col, str)]) (expandMacro expandedArgs expansionRules)
--        rest <- expandInternal mmap xs
--        return (result ++ rest)
expandInternal _ [] = return (Right [])

expand :: [Element] -> IO (Either [ErrorType] String)
expand el = return (Left [])--expandInternal Map.empty
