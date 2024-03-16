module MetricFunctionsForLanguages where

import ParadigmType
import LanguageType
import MetricFunctions

-- Concatenates two lists of paradigms.
-- This function simply appends the second list to the first, without removing duplicates.
concatParadigms :: [Paradigm] -> [Paradigm] -> [Paradigm]
concatParadigms ps1 ps2 = ps1 ++ ps2

-- Identifies paradigms shared between two lists of paradigms.
-- Returns a list of paradigms that are present in both input lists.
sharedParadigms :: [Paradigm] -> [Paradigm] -> [Paradigm]
sharedParadigms ps1 ps2 = filter (\p -> name p `elem` map name ps2) ps1

-- Generates comparison pairs, including a paradigm with itself only if it's shared.
-- This function creates pairs for comparison, allowing self-pairing only for paradigms that are shared between the two lists.
generateComparisonPairs :: [Paradigm] -> [Paradigm] -> [(Paradigm, Paradigm)]
generateComparisonPairs ps1 ps2 = let
    shared = sharedParadigms ps1 ps2
    unioned = concatParadigms ps1 ps2
    pairs = [(x, y) | x <- unioned, y <- unioned, x == y && x `elem` shared || x /= y]
  in pairs

-- Calculates the collaboration metric for two programming paradigms.
-- Utilizes the `calculateMCMetric` function to perform a comprehensive calculation of the collaboration between two paradigms.
calculateMCForLanguage :: Paradigm -> Paradigm -> IO Double
calculateMCForLanguage p1 p2 = do
    let (_, _, _, _, _, _, mcResult) = calculateMCMetric p1 p2
    return mcResult

-- Calculates the collaboration metric for two languages.
-- Adjusts the function to use generateComparisonPairs for generating pairs of paradigms for comparison.
calculateLanguageCollaborationMetric :: Language -> Language -> IO Double
calculateLanguageCollaborationMetric lang1 lang2 = do
    let ps1 = languageParadigms lang1
    let ps2 = languageParadigms lang2
    let pairs = generateComparisonPairs ps1 ps2
    scores <- mapM (uncurry calculateMCForLanguage) pairs
    let totalScore = sum scores
    let combinationsCount = fromIntegral $ length pairs
    return (totalScore / combinationsCount)


-- Writes the collaboration metric results to a CSV file.
-- Takes two languages, calculates their collaboration metric, and returns a formatted CSV string.
calculateLanguageCollaborationMetricCSV :: Language -> Language -> IO String
calculateLanguageCollaborationMetricCSV lang1 lang2 = do
    score <- calculateLanguageCollaborationMetric lang1 lang2
    return $ languageName lang1 ++ "," ++ languageName lang2 ++ "," ++ show score

-- Calculates the collaboration metric for all combinations of languages and writes the results to a CSV file.
calculateLanguageCollaborationForAllCSV :: [Language] -> FilePath -> IO ()
calculateLanguageCollaborationForAllCSV languages filePath = do
    let pairs = combinations languages
    results <- mapM (uncurry calculateLanguageCollaborationMetricCSV) pairs
    writeResultsForLanguageToCSV filePath results

-- Writes the results for language collaboration metrics to a CSV file.
-- Takes a file path and a list of strings (each representing a line in the CSV) and writes them to the file.
writeResultsForLanguageToCSV :: FilePath -> [String] -> IO ()
writeResultsForLanguageToCSV filePath results = do
    let header = "Language1,Language2,CollaborationScore\n"
    let csvData = header ++ unlines results
    writeFile filePath csvData