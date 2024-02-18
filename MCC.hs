import ParadigmData
import ParadigmType
import LanguageData
import LanguageType
import MetricFunctions

main :: IO ()
main = do
    -- Nom du fichier CSV
--    let csvFilePath = "mc_results.csv"
    -- Calculer la métrique MC pour toutes les combinaisons de paradigmes et écrire les résultats dans un fichier CSV
--    calculateMCForAllCSV paradigms csvFilePath
--    putStrLn $ "Les résultats de MC ont été écrits dans " ++ csvFilePath

    -- Nom du fichier CSV pour les résultats de collaboration entre langages
    let csvFilePath = "language_collaboration_results.csv"
    
    -- Liste des langages de programmation
    -- Assurez-vous que cette liste est définie quelque part dans votre code
    -- Exemple : let languages = [languageA, languageB, languageC]
    
    -- Calculer la métrique de collaboration pour toutes les combinaisons de langages et écrire les résultats dans un fichier CSV
    calculateLanguageCollaborationForAllCSV languages csvFilePath

    putStrLn $ "Les résultats de collaboration entre langages ont été écrits dans " ++ csvFilePath

