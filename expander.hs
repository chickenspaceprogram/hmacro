module Expander (expand, gatherEithers, convertLeft) where

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
import qualified Data.Map as Map
import Data.List
import Data.Either
import Data.Char

data MacroElement = Arg Int | Text String
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

convertLeft :: (a -> c) -> Either a b -> Either c b
convertLeft fn (Left e) = Left (fn e)
convertLeft fn (Right v) = Right (v)

gatherEithers :: [Either [a] b] -> Either [a] [b]
gatherEithers es = case lefts es of
        [] -> Right (rights es)
        ls -> Left (concat ls)

isIdStart :: Char -> Bool
isIdStart '_' = True
isIdStart '-' = True
isIdStart ch | isAsciiUpper ch || isAsciiLower ch = True
             | otherwise = False


processExpansion :: String -> MacroExpansion
processExpansion s = []

parseDefArgs :: (Int, Int) -> MacroMap -> [Element] -> Either [ErrorType] MacroExpansion
parseDefArgs (lineno, colno) mmap ((_, _, Elements name):(_, _, Elements expansion):[]) = do
        expandedName <- expandInternal mmap name
        expandedElem <- expandInternal mmap expansion
        case expandedName of
                (x:_) -> if (isIdStart x) && all (\c -> isIdStart c || isDigit c) expandedName
                         then Right (processExpansion expandedElem)
                         else Left [(lineno, colno, "Macro name `" ++ expandedName ++ "`) contains invalid characters.")]
                _ -> Left [(lineno, colno, "Cannot define a macro without a name.")]

parseDefArgs (row, col) mmap elems = Left [(row, col, "`def` macro called with an invalid number of arguments: `def` expects 2 arguments, provided " ++ show (length elems) ++ ".")]

expandInternal :: MacroMap -> [Element] -> Either [ErrorType] String
expandInternal mmap ((_, _, Plaintext str):xs) = do
        result <- expandInternal mmap xs
        Right (str ++ result)
expandInternal mmap ((_, _, Elements els):xs) = expandInternal mmap els
expandInternal mmap ((line, col, Macro (name, args)):xs) | name == "def" = do
        newexp <- parseDefArgs (line, col) mmap args
        expandInternal (Map.insert name newexp mmap) xs
                                                         | otherwise = do
        expansionRules <- ((toEither [(line, col, "Macro name `" ++ name ++ "` undefined.")]) . Map.lookup name ) mmap
        expandedArgs <- (gatherEithers . (map (\(_, _, Elements els) -> expandInternal mmap els))) args
        convertLeft (\str -> [(line, col, str)]) (expandMacro expandedArgs expansionRules)
expandInternal _ [] = Right []

expand :: [Element] -> Either [ErrorType] String
expand = expandInternal Map.empty
