module ParadigmData where

import ParadigmType

descriptiveDeclarativeProgramming :: Paradigm
descriptiveDeclarativeProgramming = Paradigm {
    concepts = ["record"],
    parents = [],
    children = ["Descriptive declarative programming","First-order functional programming"],
    turingComplete = False,
    observableND = False,
    metaParadigm = "Functional"
}

firstOrderFunctionalProgramming :: Paradigm
firstOrderFunctionalProgramming = Paradigm {
    concepts = ["record", "procedure"],
    parents = ["Descriptive declarative programming"],
    children = ["First-order functional programming","Functional programming", "Imperative programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "Functional"
}




