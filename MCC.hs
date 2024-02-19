import ParadigmData
import ParadigmType
import LanguageData
import LanguageType
import MetricFunctions
import MetricFunctionsForLanguages

main :: IO ()
main = do
    -- Path for the CSV file where MC metric results for paradigms will be written.
    let csvFilePath = "mc_results.csv"
    -- Calculate the MC metric for all combinations of paradigms and write the results to a CSV file.
    calculateMCForAllCSV paradigms csvFilePath
    putStrLn $ "Les résultats de MC ont été écrits dans " ++ csvFilePath

    -- Path for the CSV file where collaboration metric results between languages will be written.
    let csvFilePathForLanguages = "language_collaboration_results.csv"
    -- Calculate the collaboration metric for all combinations of languages and write the results to a CSV file.
    calculateLanguageCollaborationForAllCSV languages csvFilePathForLanguages
    putStrLn $ "Les résultats de collaboration entre langages ont été écrits dans " ++ csvFilePathForLanguages

