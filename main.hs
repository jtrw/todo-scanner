import Data.List
import System.Directory
import System.FilePath
import Control.Monad
import System.IO
import System.Environment
import System.Exit
import Data.Char
import Options.Applicative

data Options = Options
  { 
    dir :: FilePath,
    label :: [String]
  }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        ( long "dir"
        <> metavar "DIRECTORY"
        <> help "Directory to scan" )
    <*> many (strOption
        ( long "label"
            <> metavar "LABEL"
            <> help "Label to scan for"
 ))

main :: IO ()
main = do
    options <- execParser opts
    let dirToScan = dir options
    putStrLn $ "Scanning directory: " ++ dirToScan

    let labels = if length (label options) == 0 then getLabels else label options

    files <- getRecursiveContents dirToScan

    total <- foldM (\acc file -> do
        count <- checkFile file labels
        return (acc + count)) 0 files

    let msg = "Found " ++ show total ++ " TODOs"
    let lengthSpaces = (length msg) + 2

    if total == 0 then do
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

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path

        if isDirectory
            then getRecursiveContents path
            else if isSuffixOf ".php" path || isSuffixOf ".xml" path || isSuffixOf ".yaml" path || isSuffixOf ".yml" path
                then return [path]
                else return []
    return (concat paths)
    
checkFile :: FilePath -> [String] -> IO Int
checkFile file labels = do
    content <- readFile $ file
    let linesOfFile = lines content
    let fileLinesWithIndex = zip [1..] linesOfFile
    let fileLinesWithIndexFiltered = filter (\(i, line) -> any (`isInfixOf` line) labels) fileLinesWithIndex
    let fileLinesWithIndexFilteredMapped = map (\(i, line) -> (i, removeBeforeLabels line labels)) fileLinesWithIndexFiltered
    let fileLinesWithIndexFilteredMappedTrim = map (\(i, line) -> (i, dropWhile isSpace line)) fileLinesWithIndexFilteredMapped
    
    if length fileLinesWithIndexFilteredMappedTrim == 0
        then return 0
        else do
            putStrLn "------ --------------------------------------------------"    
            let msg = "Line: " ++ file
            putStrLn $ colorGreen msg
            mapM_ putStrLn $ map (\(i, line) -> show i ++ "     " ++ line) fileLinesWithIndexFilteredMappedTrim
            putStrLn "------ --------------------------------------------------"
            let count = length fileLinesWithIndexFilteredMappedTrim
            return count
            
getLabels :: [String]
getLabels = ["TODO", "FIXME", "XXX"]

colorGreen :: String -> String
colorGreen input = "\x1b[32m" ++ input ++ "\x1b[0m"

colorRed :: String -> String
colorRed input = "\x1b[31m" ++ input ++ "\x1b[0m"

textWithBackground :: String -> String -> String
textWithBackground color input = "\x1b[" ++ color ++ "m" ++ input ++ "\x1b[0m"

textWithRedBackroundAndBorder :: String -> String
textWithRedBackroundAndBorder input = textWithBackground "41;1;37" input

textWithRedBackground :: String -> String
textWithRedBackground input = textWithBackground "41;1;37" (" " ++ input ++ " ")

textWithGreenBackground :: String -> String
textWithGreenBackground input = textWithBackground "42;1;37" (" " ++ input ++ " ")

spacesWithRedBackground :: Int -> String
spacesWithRedBackground count = textWithBackground "41;1;37" (replicate count ' ')

spacesWithGreenBackground :: Int -> String
spacesWithGreenBackground count = textWithBackground "42;1;37" (replicate count ' ')

removeBeforeLabel :: String -> String -> String
removeBeforeLabel input label = case dropWhile (not . isPrefixOf label) (tails input) of
    (rest:_) -> rest
    _ -> input

removeBeforeLabels :: String -> [String] -> String
removeBeforeLabels input labels = foldl (\acc label -> removeBeforeLabel acc label) input labels