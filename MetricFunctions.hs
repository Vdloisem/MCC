module MetricFunctions where

import ParadigmType
import Control.Monad (forM_)

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

-- Fonction d'aide 
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

-- Génère toutes les combinaisons possibles de deux éléments d'une liste
combinations :: Eq a => [a] -> [(a, a)]
combinations list = [(x, y) | x <- list, y <- list, x /= y]

-- Calcule la métrique pour chaque combinaison de paradigmes
calculateMCForAll :: [Paradigm] -> IO ()
calculateMCForAll paradigms = do
    let pairs = combinations paradigms
    forM_ pairs $ \(p1, p2) -> do
        putStrLn $ "Calculating MC between: " ++ name p1 ++ " and " ++ name p2
        calculateMC p1 p2
