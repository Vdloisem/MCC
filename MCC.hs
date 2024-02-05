import ParadigmData
import ParadigmType
import MetricFunctions

--main :: IO ()
--main = do 
--    calculateMC descriptiveDeclarativeProgramming firstOrderFunctionalProgramming 
--    calculateMC descriptiveDeclarativeProgramming functionalProgramming
--    calculateMC descriptiveDeclarativeProgramming imperativeProgramming
--    calculateMC firstOrderFunctionalProgramming functionalProgramming
--    calculateMC firstOrderFunctionalProgramming imperativeProgramming
--    calculateMC functionalProgramming imperativeProgramming
--    calculateMC functionalProgramming functionalProgramming

main :: IO ()
main = do
    let p = paradigms
    calculateMCForAll p

