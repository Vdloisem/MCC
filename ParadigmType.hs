module ParadigmType where

-- Defines a type for concepts within a programming paradigm.
type Concept = String
-- Defines a type for the name of a programming paradigm.
type ParadigmName = String
-- Indicates whether a paradigm is Turing complete.
type TuringCompleteness = Bool
-- Indicates whether a paradigm allows observable nondeterminism.
type ObservableNondeterminism = Bool
-- Represents the meta-paradigm category (e.g., functional, imperative).
type MetaParadigm = String

-- Represents a programming paradigm with its characteristics.
data Paradigm = Paradigm {
    name :: ParadigmName,
    concepts :: [Concept],
    parents :: [ParadigmName],
    children :: [ParadigmName],
    turingComplete :: TuringCompleteness,
    observableND :: ObservableNondeterminism,
    metaParadigm :: MetaParadigm
} deriving (Eq, Show)