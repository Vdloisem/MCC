module MetricFunctions where

import ParadigmType
import LanguageType
import Control.Monad (forM_)
import Data.List (nubBy)

unionParadigms :: [Paradigm] -> [Paradigm] -> [Paradigm]
unionParadigms ps1 ps2 = ps1 ++ ps2

-- Identifie les paradigmes partagés entre deux listes de paradigmes
sharedParadigms :: [Paradigm] -> [Paradigm] -> [Paradigm]
sharedParadigms ps1 ps2 = filter (\p -> name p `elem` map name ps2) ps1

-- Génère les paires de comparaison, incluant un paradigme avec lui-même uniquement s'il est partagé
generateComparisonPairs :: [Paradigm] -> [Paradigm] -> [(Paradigm, Paradigm)]
generateComparisonPairs ps1 ps2 = let
    shared = sharedParadigms ps1 ps2
    unioned = unionParadigms ps1 ps2
    pairs = [(x, y) | x <- unioned, y <- unioned, x == y && x `elem` shared || x /= y]
  in pairs

-- Génère toutes les combinaisons possibles de deux éléments d'une liste
combinations :: Eq a => [a] -> [(a, a)]
combinations list = [(x, y) | x <- list, y <- list, x /= y]

-- Génère toutes les combinaisons possibles de deux éléments d'une liste, y compris les paires identiques
allCombinations :: Eq a => [a] -> [(a, a)]
allCombinations list = [(x, y) | x <- list, y <- list]

-- Calcule l'intersection des caractéristiques (I)
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, x `elem` ys]

union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, not (y `elem` xs)]

iFunction :: (Eq a) => [a] -> [a] -> Double
iFunction c1 c2 = let inter = intersection c1 c2
                      uni = union c1 c2
                  in fromIntegral (length inter) / fromIntegral (length uni)

-- Calcule la relation de parenté (R)
rFunction :: (Eq a) => [a] -> [a] -> Double
rFunction f s = if not (null (intersection f s)) then 1 else 0

-- Calcule la compatibilité des Méta-Paradigmes (M)
mFunction :: (Eq a) => a -> a -> Double
mFunction m1 m2 = if m1 == m2 then 1 else 0

-- Calcule la correspondance Turing-Complétude (TC)
tcFunction :: Bool -> Bool -> Double
tcFunction t1 t2 = if t1 && t2 then 1 else 0

-- Calcule la correspondance Non-Déterminisme Observable (NDO)
ndoFunction :: Bool -> Bool -> Double
ndoFunction o1 o2 = if o1 == o2 then 1 else 0

-- Calcule de la fonction MC avec impression des résultats intermédiaires
mcFunction :: (Eq a, Show a) => [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> Bool -> Bool -> Bool -> Bool -> a -> a -> Double -> Double -> Double -> Double -> Double -> Double -> String -> String -> IO ()
mcFunction c1 c2 f1 f2 ch1 ch2 t1 t2 o1 o2 m1 m2 alpha beta gamma delta epsilon zeta paradigm1 paradigm2 = do
    let iResult = alpha * iFunction c1 c2
    let r1Result = beta * rFunction f1 ch2
    let r2Result = gamma * rFunction f2 ch1
    let mResult = delta * mFunction m1 m2
    let tcResult = epsilon * tcFunction t1 t2
    let ndoResult = zeta * ndoFunction o1 o2
    let mcResult = iResult + r1Result + r2Result + mResult + tcResult + ndoResult
    putStrLn $ "-------------------------------------------------------------------------------------------------------"
    putStrLn $ "Analyzing Paradigms: " ++ paradigm1 ++ " and " ++ paradigm2
    putStrLn $ "Intersection (I): " ++ show iResult
    putStrLn $ "Parent-to-Child Relationship 1 (R1): " ++ show r1Result
    putStrLn $ "Parent-to-Child Relationship 2 (R2): " ++ show r2Result
    putStrLn $ "Meta-paradigm Compatibility (M): " ++ show mResult
    putStrLn $ "Turing Completeness (TC): " ++ show tcResult
    putStrLn $ "Observable Nondeterminism (NDO): " ++ show ndoResult
    putStrLn $ "Total MC Value: " ++ show mcResult
    putStrLn $ "-------------------------------------------------------------------------------------------------------"

mcFunctionForLanguage :: (Eq a, Show a) => [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> Bool -> Bool -> Bool -> Bool -> a -> a -> Double -> Double -> Double -> Double -> Double -> Double -> String -> String -> IO Double
mcFunctionForLanguage c1 c2 f1 f2 ch1 ch2 t1 t2 o1 o2 m1 m2 alpha beta gamma delta epsilon zeta paradigm1 paradigm2 = do
    putStrLn $ "-------------------------------------------------------------------------------------------------------"
    putStrLn $ "Analyzing Paradigms: " ++ paradigm1 ++ " and " ++ paradigm2
    putStrLn $ "Concepts P1: " ++ show c1 ++ ", Concepts P2: " ++ show c2
    putStrLn $ "Parents P1: " ++ show f1 ++ ", Parents P2: " ++ show f2
    putStrLn $ "Children P1: " ++ show ch1 ++ ", Children P2: " ++ show ch2
    putStrLn $ "Turing Complete P1: " ++ show t1 ++ ", Turing Complete P2: " ++ show t2
    putStrLn $ "Observable ND P1: " ++ show o1 ++ ", Observable ND P2: " ++ show o2
    putStrLn $ "Meta Paradigm P1: " ++ show m1 ++ ", Meta Paradigm P2: " ++ show m2

    let iResult = alpha * iFunction c1 c2
    let r1Result = beta * rFunction f1 ch2
    let r2Result = gamma * rFunction f2 ch1
    let mResult = delta * mFunction m1 m2
    let tcResult = epsilon * tcFunction t1 t2
    let ndoResult = zeta * ndoFunction o1 o2
    let mcResult = iResult + r1Result + r2Result + mResult + tcResult + ndoResult

    putStrLn $ "Intersection (I) Result: " ++ show iResult
    putStrLn $ "Parent-to-Child Relationship 1 (R1) Result: " ++ show r1Result
    putStrLn $ "Parent-to-Child Relationship 2 (R2) Result: " ++ show r2Result
    putStrLn $ "Meta-paradigm Compatibility (M) Result: " ++ show mResult
    putStrLn $ "Turing Completeness (TC) Result: " ++ show tcResult
    putStrLn $ "Observable Nondeterminism (NDO) Result: " ++ show ndoResult
    putStrLn $ "Total MC Value: " ++ show mcResult
    putStrLn $ "-------------------------------------------------------------------------------------------------------"
    
    return mcResult


-- Formater les résultats pour le CSV
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

    -- Retourner une ligne formatée pour le CSV
    return $ name p1 ++ "," ++ name p2 ++ "," ++ show iResult ++ "," ++ show r1Result ++ "," ++ show r2Result ++ "," ++ show mResult ++ "," ++ show tcResult ++ "," ++ show ndoResult ++ "," ++ show mcResult

-- Calcule la métrique MC pour deux paradigmes
calculateMC :: Paradigm -> Paradigm -> IO ()
calculateMC p1 p2 = mcFunction
    (concepts p1) (concepts p2)
    (parents p1) (parents p2)
    (children p1) (children p2)
    (turingComplete p1) (turingComplete p2)
    (observableND p1) (observableND p2)
    (metaParadigm p1) (metaParadigm p2)
    (1/6) (1/6) (1/6) (1/6) (1/6) (1/6)
    (name p1) (name p2)

-- Calcule la métrique MC pour chaque combinaison de paradigmes
calculateMCForAll :: [Paradigm] -> IO ()
calculateMCForAll paradigms = do
    let pairs = combinations paradigms
    forM_ pairs $ \(p1, p2) -> do
        putStrLn $ "Calculating MC between: " ++ name p1 ++ " and " ++ name p2
        calculateMC p1 p2

-- Fonction modifiée pour calculer MC pour toutes les combinaisons et écrire dans un fichier CSV
calculateMCForAllCSV :: [Paradigm] -> FilePath -> IO ()
calculateMCForAllCSV paradigms filePath = do
    let pairs = combinations paradigms
    results <- mapM (uncurry mcFunctionCSV) pairs
    writeResultsToCSV filePath results

-- Fonction pour écrire les résultats dans un fichier CSV
writeResultsToCSV :: FilePath -> [String] -> IO ()
writeResultsToCSV filePath results = do
    let header = "Paradigm1,Paradigm2,I,R1,R2,M,TC,NDO,TotalMC\n"
    let csvData = header ++ unlines results
    writeFile filePath csvData

-- Calcule la métrique MC pour deux paradigmes et retourne un Double
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
    let ps1 = paradigms lang1
    let ps2 = paradigms lang2
    let pairs = generateComparisonPairs ps1 ps2
    scores <- mapM (uncurry calculateMCForLanguage) pairs
    let totalScore = sum scores
    let combinationsCount = fromIntegral $ length pairs
    return (totalScore / combinationsCount)


-- Fonction pour écrire les résultats dans un fichier CSV
calculateLanguageCollaborationMetricCSV :: Language -> Language -> IO String
calculateLanguageCollaborationMetricCSV lang1 lang2 = do
    score <- calculateLanguageCollaborationMetric lang1 lang2
    return $ languageName lang1 ++ "," ++ languageName lang2 ++ "," ++ show score

-- Calcule la métrique de collaboration pour toutes les combinaisons de langages
calculateLanguageCollaborationForAllCSV :: [Language] -> FilePath -> IO ()
calculateLanguageCollaborationForAllCSV languages filePath = do
    let pairs = combinations languages
    results <- mapM (uncurry calculateLanguageCollaborationMetricCSV) pairs
    writeResultsForLanguageToCSV filePath results

-- Fonction pour écrire les résultats dans un fichier CSV
writeResultsForLanguageToCSV :: FilePath -> [String] -> IO ()
writeResultsForLanguageToCSV filePath results = do
    let header = "Language1,Language2,CollaborationScore\n"
    let csvData = header ++ unlines results
    writeFile filePath csvData


