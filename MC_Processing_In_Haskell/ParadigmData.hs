module ParadigmData where

import ParadigmType

-- L1 Paradigms (checked)
descriptiveDeclarativeProgramming :: Paradigm
descriptiveDeclarativeProgramming = Paradigm {
    name = "descriptive declarative programming",
    concepts = ["record"],
    parents = ["descriptive declarative programming"],
    children = ["descriptive declarative programming", "first-order functional programming"],
    turingComplete = False,
    observableND = False,
    metaParadigm = "functional"
}

-- L2 Paradigms (checked)
firstOrderFunctionalProgramming :: Paradigm
firstOrderFunctionalProgramming = Paradigm {
    name = "first-order functional programming",
    concepts = ["record", "procedure"],
    parents = ["descriptive declarative programming", "first-order functional programming"],
    children = ["first-order functional programming", "functional programming", "imperative programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "functional"
}

-- L3 Paradigms (checked)
functionalProgramming :: Paradigm
functionalProgramming = Paradigm {
    name = "functional programming",
    concepts = ["record", "procedure", "closure"],
    parents = ["first-order functional programming", "functional programming"],
    children = ["functional programming", "deterministic logic programming", "lazy functional programming", 
                "declarative concurrent programming",  
                "continuation programming", "adt functional programming", 
                "event loop programming", "stateful functional programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "functional"
}

imperativeProgramming :: Paradigm
imperativeProgramming = Paradigm {
    name = "imperative programming",
    concepts = ["record", "procedure", "cell"],
    parents = ["first-order functional programming", "imperative programming"],
    children = ["imperative programming", "sequential object oriented programming",
                "guarded command programming", "imperative search programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "shared state"
}

-- L4 Paradigms (checked)
deterministicLogicProgramming :: Paradigm
deterministicLogicProgramming = Paradigm {
    name = "deterministic logic programming",
    concepts = ["record", "procedure", "closure", "unification"],
    parents = ["functional programming", "deterministic logic programming"],
    children = ["deterministic logic programming", "relational and logic programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "logic and constraint"
}

lazyFunctionalProgramming :: Paradigm
lazyFunctionalProgramming = Paradigm {
    name = "lazy functional programming",
    concepts = ["record", "procedure", "closure", "by-need sync"],
    parents = ["functional programming", "lazy functional programming"],
    children = ["lazy functional programming", "lazy declarative concurrent programming", "lazy dataflow programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "functional"
}

continuationProgramming :: Paradigm
continuationProgramming = Paradigm {
    name = "continuation programming",
    concepts = ["record", "procedure", "closure", "continuation"],
    parents = ["functional programming", "continuation programming"],
    children = ["continuation programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "functional"
}

adtFunctionalProgramming :: Paradigm
adtFunctionalProgramming = Paradigm {
    name = "adt functional programming",
    concepts = ["record", "procedure", "closure", "unforgeable constant"],
    parents = ["functional programming", "adt functional programming"],
    children = ["adt functional programming", "adt imperative programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "data abstraction"
}

eventLoopProgramming :: Paradigm
eventLoopProgramming = Paradigm {
    name = "event loop programming",
    concepts = ["record", "procedure", "closure", "channel"],
    parents = ["functional programming", "event loop programming"],
    children = ["event loop programming", "multi agent programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "message passing"
}

statefulFunctionalProgramming :: Paradigm
statefulFunctionalProgramming = Paradigm {
    name = "stateful functional programming",
    concepts = ["record", "procedure", "closure", "cell"],
    parents = ["functional programming", "stateful functional programming"],
    children = ["stateful functional programming", "shared state concurrent programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "shared state"
}

guardedCommandProgramming :: Paradigm
guardedCommandProgramming = Paradigm {
    name = "guarded command programming",
    concepts = ["record", "procedure", "cell", "nondet choice"],
    parents = ["imperative programming", "guarded command programming"],
    children = ["guarded command programming"],
    turingComplete = True,
    observableND = True,
    metaParadigm = "constrained execution"
}

imperativeSearchProgramming :: Paradigm
imperativeSearchProgramming = Paradigm {
    name = "imperative search programming",
    concepts = ["record", "procedure", "cell", "search"],
    parents = ["imperative programming", "imperative search programming"],
    children = ["imperative search programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "algorithmic search"
}

sequentialObjectOrientedProgramming :: Paradigm
sequentialObjectOrientedProgramming = Paradigm {
    name = "sequential object oriented programming",
    concepts = ["record", "procedure", "cell", "closure"],
    parents = ["imperative programming", "sequential object oriented programming"],
    children = ["sequential object oriented programming", "concurrent object oriented programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "shared state"
}

-- L5 Paradigms (checked)

relationalAndLogicProgramming :: Paradigm
relationalAndLogicProgramming = Paradigm {
    name = "relational and logic programming",
    concepts = ["record", "procedure", "closure", "unification", "search"],
    parents = ["deterministic logic programming","relational and logic programming"],
    children = ["relational and logic programming","constraint logic programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "logic and constraint"
}

declarativeConcurrentProgramming :: Paradigm
declarativeConcurrentProgramming = Paradigm {
    name = "declarative concurrent programming",
    concepts = ["record", "procedure", "closure", "thread", "single assign"],
    parents = ["functional programming","declarative concurrent programming"],
    children = ["declarative concurrent programming","lazy declarative concurrent programming","nonmonotonic dataflow programming", "multi agent dataflow programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "concurrent dataflow"
}

adtImperativeProgramming :: Paradigm
adtImperativeProgramming = Paradigm {
    name = "adt imperative programming",
    concepts = ["record", "procedure", "closure", "unforgeable constant", "cell"],
    parents = ["adt functional programming","adt imperative programming"],
    children = ["adt imperative programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "shared state"
}

multiAgentProgramming :: Paradigm
multiAgentProgramming = Paradigm {
    name = "multi agent programming",
    concepts = ["record", "procedure", "closure", "channel", "thread"],
    parents = ["event loop programming","multi agent programming"],
    children = ["multi agent programming","active object programming"],
    turingComplete = True,
    observableND = True,
    metaParadigm = "message passing"
}

sharedStateConcurrentProgramming :: Paradigm
sharedStateConcurrentProgramming = Paradigm {
    name = "shared state concurrent programming",
    concepts = ["record", "procedure", "closure", "cell", "thread"],
    parents = ["stateful functional programming","shared state concurrent programming"],
    children = ["shared state concurrent programming"],
    turingComplete = True,
    observableND = True,
    metaParadigm = "shared state"
}

concurrentObjectOrientedProgramming :: Paradigm
concurrentObjectOrientedProgramming = Paradigm {
    name = "concurrent object oriented programming",
    concepts = ["record", "procedure", "cell", "closure", "thread"],
    parents = ["sequential object oriented programming","concurrent object oriented programming"],
    children = ["concurrent object oriented programming"],
    turingComplete = True,
    observableND = True,
    metaParadigm = "shared state"
}

-- L6 Paradigms (checked)
constraintLogicProgramming :: Paradigm
constraintLogicProgramming = Paradigm {
    name = "constraint logic programming",
    concepts = ["record", "procedure", "closure", "unification", "search", "solver"],
    parents = ["relational and logic programming","constraint logic programming"],
    children = ["constraint logic programming","concurrent constraint programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "logic and constraint"
}

lazyDeclarativeConcurrentProgramming :: Paradigm
lazyDeclarativeConcurrentProgramming = Paradigm {
    name = "lazy declarative concurrent programming",
    concepts = ["record", "procedure", "closure", "thread", "single assign", "by-need sync"],
    parents = ["lazy functional programming", "declarative concurrent programming","lazy declarative concurrent programming"],
    children = ["lazy declarative concurrent programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "functional"
}

nonmonotonicDataflowProgramming :: Paradigm
nonmonotonicDataflowProgramming = Paradigm {
    name = "nonmonotonic dataflow programming",
    concepts = ["record", "procedure", "closure", "thread", "single assign", "nondet choice"],
    parents = ["declarative concurrent programming","nonmonotonic dataflow programming"],
    children = ["nonmonotonic dataflow programming", "continuous synchronous programming"],
    turingComplete = True,
    observableND = True,
    metaParadigm = "reactive"
}

multiAgentDataflowProgramming :: Paradigm
multiAgentDataflowProgramming = Paradigm {
    name = "multi agent dataflow programming",
    concepts = ["record", "procedure", "closure", "thread", "single assign", "channel"],
    parents = ["declarative concurrent programming","multi agent dataflow programming"],
    children = ["multi agent dataflow programming"],
    turingComplete = True,
    observableND = True,
    metaParadigm = "dataflow and message passing"
}

activeObjectProgramming :: Paradigm
activeObjectProgramming = Paradigm {
    name = "active object programming",
    concepts = ["record", "procedure", "closure", "channel", "thread", "local cell"],
    parents = ["multi agent programming","active object programming"],
    children = ["active object programming"],
    turingComplete = True,
    observableND = True,
    metaParadigm = "message passing"
}

-- L7 Paradigms (checked)

concurrentConstraintProgramming :: Paradigm
concurrentConstraintProgramming = Paradigm {
    name = "concurrent constraint programming",
    concepts = ["record", "procedure", "closure", "unification", "search", "solver", "thread"],
    parents = ["constraint logic programming","concurrent constraint programming"],
    children = ["concurrent constraint programming","lazy concurrent constraint programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "logic and constraint"
}

continuousSynchronousProgramming :: Paradigm
continuousSynchronousProgramming = Paradigm {
    name = "continuous synchronous programming",
    concepts = ["record", "procedure", "closure", "thread", "single assign", "nondet choice", "sync on partial termination"],
    parents = ["nonmonotonic dataflow programming", "continuous synchronous programming"],
    children = ["continuous synchronous programming","discrete synchronous programming"],
    turingComplete = True,
    observableND = True,
    metaParadigm = "reactive"
}

-- L8 Paradigms

lazyConcurrentConstraintProgramming :: Paradigm
lazyConcurrentConstraintProgramming = Paradigm {
    name = "lazy concurrent constraint programming",
    concepts = ["record", "procedure", "closure", "unification", "search", "solver", "thread", "by-need sync"],
    parents = ["concurrent constraint programming","lazy concurrent constraint programming"],
    children = ["lazy concurrent constraint programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "logic and constraint"
}

discreteSynchronousProgramming :: Paradigm
discreteSynchronousProgramming = Paradigm {
    name = "discrete synchronous programming",
    concepts = ["record", "procedure", "closure", "thread", "single assign", "nondet choice", "sync on partial termination", "clocked computation"],
    parents = ["continuous synchronous programming","discrete synchronous programming"],
    children = ["discrete synchronous programming"],
    turingComplete = True,
    observableND = False,
    metaParadigm = "reactive"
}

paradigms :: [Paradigm]
paradigms = [
    descriptiveDeclarativeProgramming,
    firstOrderFunctionalProgramming,
    functionalProgramming,
    imperativeProgramming,
    deterministicLogicProgramming,
    lazyFunctionalProgramming,
    continuationProgramming,
    adtFunctionalProgramming,
    eventLoopProgramming,
    statefulFunctionalProgramming,
    guardedCommandProgramming,
    imperativeSearchProgramming,
    sequentialObjectOrientedProgramming,
    relationalAndLogicProgramming,
    declarativeConcurrentProgramming,
    adtImperativeProgramming,
    multiAgentProgramming,
    sharedStateConcurrentProgramming,
    concurrentObjectOrientedProgramming,
    constraintLogicProgramming,
    lazyDeclarativeConcurrentProgramming,
    nonmonotonicDataflowProgramming,
    multiAgentDataflowProgramming,
    activeObjectProgramming,
    concurrentConstraintProgramming,
    continuousSynchronousProgramming,
    lazyConcurrentConstraintProgramming,
    discreteSynchronousProgramming
    ]
