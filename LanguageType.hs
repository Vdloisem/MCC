module LanguageType where

import ParadigmType

data Language = Language {
    languageName :: String,
    paradigms :: [Paradigm]
} deriving (Eq, Show)
