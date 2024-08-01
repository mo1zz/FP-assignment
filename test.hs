import Data.Char (isAlpha, toLower)
import Data.List (group, sort, intercalate, nub)
import qualified Data.Map as Map
import System.IO (readFile, writeFile)
import Control.Exception (try, IOException)

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
    -- Create a list of (lineNumber, word) pairs
    wordsWithLines = concatMap (\(lineNum, line) -> 
                                  zip (repeat lineNum) (map normalizeWord . nub $ splitWords line)) 
                               (zip [1..] lines)
    
    -- Create index map: word -> list of line numbers
    indexMap = Map.fromListWith (++) [(word, [lineNum]) | (lineNum, word) <- wordsWithLines]
    
    -- Create frequency map: word -> count
    freqMap = Map.fromListWith (+) [(word, 1) | (_, word) <- wordsWithLines]

-- Convert index map to sorted string format for index.txt
formatIndexMap :: Map.Map String [Int] -> String
formatIndexMap indexMap = unlines $
    ["This file contains an alphabetical list of words from the input text.",
     "Each line shows a word followed by the line numbers where it appears.",
     "Format: word line_number1, line_number2, ...",
     ""] ++  -- Empty line for separation
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
     ""] ++  -- Empty line for separation
    [word ++ " " ++ show count 
    | (count, words) <- sortedFreqList, word <- sort words]
  where
    -- Sort by decreasing frequency, then alphabetically for same frequency
    sortedFreqList = reverse $ sort [(count, [word | (word, cnt) <- wordList, cnt == count]) 
                                    | count <- nub (map snd wordList)]
    wordList = Map.toList freqMap
    nub = map head . group . sort

-- Main function to read input, process text, and write output files
main :: IO ()
main = do
    -- Read input file with error handling
    inputResult <- try (readFile "/Users/moiz/Desktop/FP-assignment/input.txt") :: IO (Either IOException String)
    case inputResult of
        Left err -> putStrLn $ "Error reading input file: " ++ show err
        Right content -> do
            let linesOfText = lines content
                (indexMap, freqMap) = processText linesOfText
                indexOutput = formatIndexMap indexMap
                freqOutput = formatFreqMap freqMap
            -- Write index.txt with error handling
            indexResult <- try (writeFile "index.txt" indexOutput) :: IO (Either IOException ())
            case indexResult of
                Left err -> putStrLn $ "Error writing index.txt: " ++ show err
                Right _ -> putStrLn "Successfully wrote index.txt"
            -- Write frequency.txt with error handling
            freqResult <- try (writeFile "frequency.txt" freqOutput) :: IO (Either IOException ())
            case freqResult of
                Left err -> putStrLn $ "Error writing frequency.txt: " ++ show err
                Right _ -> putStrLn "Successfully wrote frequency.txt"
