import ParadigmData
import ParadigmType
import MetricFunctions

main :: IO ()
main = do
    -- Nom du fichier CSV
    let csvFilePath = "mc_results.csv"
    
    -- Calculer la métrique MC pour toutes les combinaisons de paradigmes et écrire les résultats dans un fichier CSV
    calculateMCForAllCSV paradigms csvFilePath

    putStrLn $ "Les résultats de MC ont été écrits dans " ++ csvFilePath


