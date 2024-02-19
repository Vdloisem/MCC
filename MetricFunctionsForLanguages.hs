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

-- Calculates the MC metric for two paradigms and returns a Double.
-- This function applies the MC calculation logic to two paradigms and returns the resulting metric as a Double.
mcFunctionForLanguage :: (Eq a, Show a) => [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> Bool -> Bool -> Bool -> Bool -> a -> a -> Double -> Double -> Double -> Double -> Double -> Double -> String -> String -> IO Double
mcFunctionForLanguage c1 c2 f1 f2 ch1 ch2 t1 t2 o1 o2 m1 m2 alpha beta gamma delta epsilon zeta paradigm1 paradigm2 = do
    let iResult = alpha * iFunction c1 c2
    let r1Result = beta * rFunction f1 ch2
    let r2Result = gamma * rFunction f2 ch1
    let mResult = delta * mFunction m1 m2
    let tcResult = epsilon * tcFunction t1 t2
    let ndoResult = zeta * ndoFunction o1 o2
    let mcResult = iResult + r1Result + r2Result + mResult + tcResult + ndoResult
    
    return mcResult

-- Calculates the collaboration metric for two languages.
-- Adjusts the function to use generateComparisonPairs for generating pairs of paradigms for comparison.
calculateMCForLanguage :: Paradigm -> Paradigm -> IO Double
calculateMCForLanguage p1 p2 = mcFunctionForLanguage
    (concepts p1) (concepts p2)
    (parents p1) (parents p2)
    (children p1) (children p2)
    (turingComplete p1) (turingComplete p2)
    (observableND p1) (observableND p2)
    (metaParadigm p1) (metaParadigm p2)
    (1/6) (1/6) (1/6) (1/6) (1/6) (1/6)
    (name p1) (name p2)

-- Calcule la métrique de collaboration pour deux langages
-- Ajustez la fonction calculateLanguageCollaborationMetric pour utiliser generateComparisonPairs
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