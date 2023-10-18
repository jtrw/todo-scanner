import Data.List
import System.Directory
import System.FilePath
import Control.Monad
--import System.IO
--import System.Environment
import System.Exit
import Data.Char
import Options.Applicative
import Data.Bifunctor (second)
--import Data.Binary.Put (PutM(Put))

data Options = Options
  {
    dir :: [FilePath],
    label :: [String],
    ext :: [String]
  }

optionsParser :: Parser Options
optionsParser = Options
    <$> many (strOption
        ( long "dir"
        <> metavar "DIRECTORY"
        <> help "Directory to scan" ))
    <*> many (strOption
        ( long "label"
            <> metavar "LABEL"
            <> help "Label to scan for"
        ))
    <*> many (strOption
    ( long "exts"
        <> metavar "EXTENSION"
        <> help "File extension to scan for"
    ))

main :: IO ()
main = do
    options <- execParser opts

    let  dirsToScan = dir options

    if null dirsToScan
        then do
            putStrLn $ colorRed "No directory to scan. Use --dir option"
            exitFailure
        else do
            return ()

    mapM_ (putStrLn . (\ currentDir -> colorBlue ("Scanning directory: " ++ currentDir))) dirsToScan

    let labels = if null (label options) then getDefaultLabels else label options
    let extensions = if null (ext options) then getDedaultExtensions else ext options

    files <- foldM (\acc currentDir -> do
        files <- getRecursiveContents currentDir extensions
        return (acc ++ files)) [] dirsToScan

    total <- foldM (\acc file -> do
        count <- checkFile file labels
        return (acc + count)) 0 files

    let msg = "Found " ++ show total ++ " TODOs"
    let lengthSpaces = length msg + 2

    _ <- if total == 0 then do
        putStrLn $ spacesWithGreenBackground lengthSpaces
        putStrLn $ textWithGreenBackground msg
        putStrLn $ spacesWithGreenBackground lengthSpaces
        exitSuccess
    else do
        putStrLn $ spacesWithRedBackground lengthSpaces
        putStrLn $ textWithRedBackground msg
        putStrLn $ spacesWithRedBackground lengthSpaces
        exitFailure
    exitSuccess
    where
        opts = info (helper <*> optionsParser)
          ( fullDesc
         <> progDesc "Scan directory for TODOs"
         <> header "todo - a simple TODO scanner" )

getRecursiveContents :: FilePath -> [String] -> IO [FilePath]
getRecursiveContents topdir extensions = do
    isDirectoryExist <- doesDirectoryExist topdir
    if not isDirectoryExist
        then do
            putStrLn $ colorRed ("Directory " ++ topdir ++ " does not exist")
            exitFailure
        else do
            names <- getDirectoryContents topdir
            let properNames = filter (`notElem` [".", ".."]) names
            paths <- forM properNames $ \name -> do
                let path = topdir </> name
                isDirectory <- doesDirectoryExist path

                if isDirectory
                    then getRecursiveContents path extensions
                    else if any (`isSuffixOf` path) extensions
                        then return [path]
                        else return []
            return (concat paths)

checkFile :: FilePath -> [String] -> IO Int
checkFile file labels = do
    content <- readFile file
    let linesOfFile = lines content
    let fileLinesWithIndex = zip [1..] linesOfFile
    let fileLinesWithIndexFiltered = filter (\(_, line) -> any (`isInfixOf` line) labels) fileLinesWithIndex
    let fileLinesWithIndexFilteredMapped = map (\(i, line) -> (i, removeBeforeLabels line labels)) fileLinesWithIndexFiltered
    let fileLinesWithIndexFilteredMappedTrim = map (Data.Bifunctor.second (dropWhile isSpace)) fileLinesWithIndexFilteredMapped

    if null fileLinesWithIndexFilteredMappedTrim
        then return 0
        else do
            putStrLn "------ --------------------------------------------------"
            let msg = "Line: " ++ file
            putStrLn $ colorGreen msg
            mapM_ (putStrLn . (\(i, line) -> show (i :: Integer) ++ "     " ++ line)) fileLinesWithIndexFilteredMappedTrim
            putStrLn "------ --------------------------------------------------"
            let count = length fileLinesWithIndexFilteredMappedTrim
            return count

getDefaultLabels :: [String]
getDefaultLabels = ["TODO", "FIXME", "XXX"]

getDedaultExtensions :: [String]
getDedaultExtensions = ["php", "js", "xml", "yml", "yaml"]

colorGreen :: String -> String
colorGreen input = "\x1b[32m" ++ input ++ "\x1b[0m"

colorRed :: String -> String
colorRed input = "\x1b[31m" ++ input ++ "\x1b[0m"

textWithBackground :: String -> String -> String
textWithBackground color input = "\x1b[" ++ color ++ "m" ++ input ++ "\x1b[0m"

colorBlue :: String -> String
colorBlue input = "\x1b[34m" ++ input ++ "\x1b[0m"

textWithRedBackground :: String -> String
textWithRedBackground input = textWithBackground "41;1;37" (" " ++ input ++ " ")

textWithGreenBackground :: String -> String
textWithGreenBackground input = textWithBackground "42;1;37" (" " ++ input ++ " ")

spacesWithRedBackground :: Int -> String
spacesWithRedBackground count = textWithBackground "41;1;37" (replicate count ' ')

spacesWithGreenBackground :: Int -> String
spacesWithGreenBackground count = textWithBackground "42;1;37" (replicate count ' ')

removeBeforeLabel :: String -> String -> String
removeBeforeLabel input targetLabel = case dropWhile (not . isPrefixOf targetLabel) (tails input) of
    (rest:_) -> rest
    _ -> input

removeBeforeLabels :: String -> [String] -> String
removeBeforeLabels = foldl removeBeforeLabel