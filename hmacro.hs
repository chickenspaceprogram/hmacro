import Expander
import Parser
import Iohandling
import System.IO
import Debug.Trace

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

convertErrMsgs :: [ErrorType] -> String
convertErrMsgs errs = concat (map (\(lineno, colno, msg) -> "\x1b[1;31mError:\x1b[39m Line " ++ (show lineno) ++ ", Col " ++ (show colno) ++ ": " ++ msg ++ "\x1b[22m\n") errs)


parseToTxt :: String -> IO (Either [ErrorType] String)
parseToTxt s = expand `unwrapEither` parse s


parseAll :: [String] -> IO (Either String String)
parseAll strs = (convertErrMsgs `fmapLeft`) <$> (concat <$>) <$> (gatherEithers) <$> (mapM (parseToTxt) strs)

helpMsg :: String
helpMsg = "Usage:\n\n\
\hmacro [OPTIONS] <filename> [-o OUTFILE]\n\n\
\`-v`, `--version` - Print a version message, then exit\n\
\`--help`          - Print a help message, then exit\n\
\`-o`, `--output`  - Specify a filename to output to (default: write to stdout)\n\
\`--license`       - Display information about hmacro's license\n\n\
\You can pass a list of filenames to hmacro.\n\
\They will each be expanded separately and the results concatenated.\n"

versionMsg :: String
versionMsg = "hmacro 1.0 Copyright (C) 2025 Athena Boose\n\n\
\This program comes with ABSOLUTELY NO WARRANTY; for details try `hmacro --license`.\n\
\This is free software, and you are welcome to redistribute it\n\
\under certain conditions; try `hmacro --license` for details.\n"


licenseMsg :: String
licenseMsg = "hmacro - a macro-expander written in Haskell\n\
\Copyright (C) 2025 Athena Boose\n\n\
\This program is free software: you can redistribute it and/or modify\n\
\it under the terms of the GNU General Public License as published by\n\
\the Free Software Foundation, either version 3 of the License, or\n\
\(at your option) any later version.\n\n\
\This program is distributed in the hope that it will be useful,\n\
\but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
\MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
\GNU General Public License for more details.\n\n\
\You should have received a copy of the GNU General Public License\n\
\along with this program.  If not, see <https://www.gnu.org/licenses/>.\n"

parseFiles :: CliArgs -> IO (Either String (String, Maybe String))
parseFiles Help = return (Right (helpMsg, Nothing))
parseFiles Version = return (Right (versionMsg, Nothing))
parseFiles License = return (Right (licenseMsg, Nothing))
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

