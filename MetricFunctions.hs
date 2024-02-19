module MetricFunctions where

import ParadigmType
import LanguageType

-- Generates all possible pairs of elements from a list.
-- Each element is paired with every other element, excluding self-pairing.
combinations :: Eq a => [a] -> [(a, a)]
combinations list = [(x, y) | x <- list, y <- list, x /= y]

-- Calculates the intersection of two lists.
-- Returns elements that are present in both lists.
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, x `elem` ys]

-- Calculates the union of two lists.
-- Returns a list of unique elements present in either list.
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, not (y `elem` xs)]

-- Calculates the I function, representing the intersection over union metric.
-- This metric measures the similarity between two sets of concepts.
iFunction :: (Eq a) => [a] -> [a] -> Double
iFunction c1 c2 = let inter = intersection c1 c2
                      uni = union c1 c2
                  in fromIntegral (length inter) / fromIntegral (length uni)

-- Calculates the R function, representing the relationship metric.
-- Returns 1 if there is at least one common element between two lists, otherwise 0.
rFunction :: (Eq a) => [a] -> [a] -> Double
rFunction f s = if not (null (intersection f s)) then 1 else 0

-- Calculates the M function, representing the meta-paradigm compatibility metric.
-- Returns 1 if two meta-paradigms are the same, otherwise 0.
mFunction :: (Eq a) => a -> a -> Double
mFunction m1 m2 = if m1 == m2 then 1 else 0

-- Calculates the TC function, representing the Turing-completeness correspondence.
-- Returns 1 if both inputs are Turing-complete, otherwise 0.
tcFunction :: Bool -> Bool -> Double
tcFunction t1 t2 = if t1 && t2 then 1 else 0

-- Calculates the NDO function, representing the observable nondeterminism correspondence.
-- Returns 1 if both inputs allow observable nondeterminism, otherwise 0.
ndoFunction :: Bool -> Bool -> Double
ndoFunction o1 o2 = if o1 == o2 then 1 else 0

-- Calculates the MC metric.
-- Takes two paradigms and calculates the MC metric, returning a formatted CSV string.
mcFunctionCSV :: Paradigm -> Paradigm -> IO String
mcFunctionCSV p1 p2 = do
    let (c1, c2) = (concepts p1, concepts p2)
    let (f1, f2) = (parents p1, parents p2)
    let (ch1, ch2) = (children p1, children p2)
    let (t1, t2) = (turingComplete p1, turingComplete p2)
    let (o1, o2) = (observableND p1, observableND p2)
    let (m1, m2) = (metaParadigm p1, metaParadigm p2)

    let iResult = (1/6) * iFunction c1 c2
    let r1Result = (1/6) * rFunction f1 ch2
    let r2Result = (1/6) * rFunction f2 ch1
    let mResult = (1/6) * mFunction m1 m2
    let tcResult = (1/6) * tcFunction t1 t2
    let ndoResult = (1/6) * ndoFunction o1 o2
    let mcResult = iResult + r1Result + r2Result + mResult + tcResult + ndoResult

    return $ name p1 ++ "," ++ name p2 ++ "," ++ show iResult ++ "," ++ show r1Result ++ "," ++ show r2Result ++ "," ++ show mResult ++ "," ++ show tcResult ++ "," ++ show ndoResult ++ "," ++ show mcResult

-- Modified function to calculate MC for all combinations and write to a CSV file.
calculateMCForAllCSV :: [Paradigm] -> FilePath -> IO ()
calculateMCForAllCSV paradigms filePath = do
    let pairs = combinations paradigms
    results <- mapM (uncurry mcFunctionCSV) pairs
    writeResultsToCSV filePath results

-- Writes the results to a CSV file.
-- Takes a file path and a list of strings (each representing a line in the CSV) and writes them to the file.
writeResultsToCSV :: FilePath -> [String] -> IO ()
writeResultsToCSV filePath results = do
    let header = "Paradigm1,Paradigm2,I,R1,R2,M,TC,NDO,TotalMC\n"
    let csvData = header ++ unlines results
    writeFile filePath csvData