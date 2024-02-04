module ParadigmType where

type Concept = String
type ParadigmName = String
type TuringCompleteness = Bool
type ObservableNondeterminism = Bool
type MetaParadigm = String

data Paradigm = Paradigm {
    concepts :: [Concept],
    parents :: [ParadigmName],
    children :: [ParadigmName],
    turingComplete :: TuringCompleteness,
    observableND :: ObservableNondeterminism,
    metaParadigm :: MetaParadigm
} deriving (Show)