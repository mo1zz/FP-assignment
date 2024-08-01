import Data.Char (isAlpha, toLower)
import Data.List (group, sort, intercalate, nub)
import qualified Data.Map as Map
import System.IO (readFile, writeFile)
import Control.Exception (try, IOException)
import System.Environment (getArgs)
import System.Directory (doesFileExist)

-- Normalize a word by converting it to lowercase and removing unwanted characters
normalizeWord :: String -> String
normalizeWord = map toLower . filter (\c -> isAlpha c || c == '-' || c == '\'')

-- Split a string into words, removing any empty strings resulting from split
splitWords :: String -> [String]
splitWords = filter (not . null) . words . map (\c -> if isAlpha c || c == '-' || c == '\'' then c else ' ')

-- Process the input text to get word indices and frequencies
processText :: [String] -> (Map.Map String [Int], Map.Map String Int)
processText lines = (indexMap, freqMap)
  where
    wordsWithLines = concatMap (\(lineNum, line) -> 
                                  zip (repeat lineNum) (map normalizeWord . nub $ splitWords line)) 
                               (zip [1..] lines)
    indexMap = Map.fromListWith (++) [(word, [lineNum]) | (lineNum, word) <- wordsWithLines]
    freqMap = Map.fromListWith (+) [(word, 1) | (_, word) <- wordsWithLines]

-- Convert index map to sorted string format for index.txt
formatIndexMap :: Map.Map String [Int] -> String
formatIndexMap indexMap = unlines $
    ["This file contains an alphabetical list of words from the input text.",
     "Each line shows a word followed by the line numbers where it appears.",
     "Format: word line_number1, line_number2, ...",
     ""] ++
    [word ++ " " ++ (intercalate ", " (map show (sort lineNums))) 
    | (word, lineNums) <- sortedIndexList]
  where
    sortedIndexList = sort [(word, lineNums) | (word, lineNums) <- Map.toList indexMap]

-- Convert frequency map to sorted string format for frequency.txt
formatFreqMap :: Map.Map String Int -> String
formatFreqMap freqMap = unlines $
    ["This file contains a list of words from the input text sorted by frequency.",
     "Words are listed in descending order of frequency.",
     "Words with the same frequency are sorted alphabetically.",
     "Format: word frequency",
     ""] ++
    [word ++ " " ++ show count 
    | (count, words) <- sortedFreqList, word <- sort words]
  where
    sortedFreqList = reverse $ sort [(count, [word | (word, cnt) <- wordList, cnt == count]) 
                                    | count <- nub (map snd wordList)]
    wordList = Map.toList freqMap

-- Main function to read input, process text, and write output files
main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> processFile inputFile "index.txt" "frequency.txt"
        [inputFile, indexFile, freqFile] -> processFile inputFile indexFile freqFile
        _ -> putStrLn "Usage: runhaskell Assignment.hs <input_file> [<index_output_file> <frequency_output_file>]"

processFile :: FilePath -> FilePath -> FilePath -> IO ()
processFile inputFile indexFile freqFile = do
    inputExists <- doesFileExist inputFile
    if not inputExists
        then putStrLn $ "Error: Input file '" ++ inputFile ++ "' does not exist."
        else do
            inputResult <- try (readFile inputFile) :: IO (Either IOException String)
            case inputResult of
                Left err -> putStrLn $ "Error reading input file: " ++ show err
                Right content -> do
                    let linesOfText = lines content
                        (indexMap, freqMap) = processText linesOfText
                        indexOutput = formatIndexMap indexMap
                        freqOutput = formatFreqMap freqMap
                    
                    writeOutput indexFile indexOutput
                    writeOutput freqFile freqOutput

writeOutput :: FilePath -> String -> IO ()
writeOutput filePath content = do
    result <- try (writeFile filePath content) :: IO (Either IOException ())
    case result of
        Left err -> putStrLn $ "Error writing " ++ filePath ++ ": " ++ show err
        Right _ -> putStrLn $ "Successfully wrote " ++ filePath