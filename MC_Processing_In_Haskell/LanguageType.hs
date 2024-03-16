module LanguageType where

import ParadigmType

-- Represents a programming language with its associated paradigms.
data Language = Language {
    languageName :: String,
    languageParadigms :: [Paradigm]
} deriving (Eq, Show)
