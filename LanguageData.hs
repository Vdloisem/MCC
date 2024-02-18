module LanguageData where

import LanguageType
import ParadigmData

xml :: Language
xml = Language "XML" [descriptiveDeclarativeProgramming]

sexpression :: Language
sexpression = Language "S-expression" [descriptiveDeclarativeProgramming]

scheme :: Language
scheme = Language "Scheme" [functionalProgramming,
                            continuationProgramming]

ml :: Language
ml = Language "ML" [functionalProgramming,
                    continuationProgramming,
                    adtFunctionalProgramming]

prolog :: Language
prolog = Language "Prolog" [deterministicLogicProgramming,
                            imperativeSearchProgramming]

clp :: Language
clp = Language "CLP" [constraintLogicProgramming]

life :: Language
life = Language "LIFE" [concurrentConstraintProgramming]

akl :: Language
akl = Language "AKL" [concurrentConstraintProgramming,
                      nonmonotonicDataflowProgramming,
                      multiAgentDataflowProgramming,
                      multiAgentProgramming]

oz :: Language
oz = Language "Oz" [lazyConcurrentConstraintProgramming,
                    lazyDeclarativeConcurrentProgramming,
                    nonmonotonicDataflowProgramming,
                    multiAgentDataflowProgramming,
                    activeObjectProgramming,
                    concurrentObjectOrientedProgramming,
                    adtImperativeProgramming]

alice :: Language
alice = Language "Alice" [lazyConcurrentConstraintProgramming,
                          lazyDeclarativeConcurrentProgramming,
                          nonmonotonicDataflowProgramming,
                          multiAgentDataflowProgramming,
                          activeObjectProgramming,
                          concurrentObjectOrientedProgramming]

pcurry :: Language
pcurry = Language "Curry" [lazyConcurrentConstraintProgramming,
                          lazyDeclarativeConcurrentProgramming,
                          nonmonotonicDataflowProgramming]

hasKell :: Language
hasKell = Language "HasKell" [lazyFunctionalProgramming,adtFunctionalProgramming]

pipes :: Language
pipes = Language "Pipes" [monotonicDataflowProgramming]

e :: Language
e = Language "E" [adtFunctionalProgramming,
                  activeObjectProgramming]

fcp :: Language
fcp = Language "FCP" [nonmonotonicDataflowProgramming]

frTime :: Language
frTime = Language "FrTime" [continuousSynchronousProgramming]

esterel :: Language
esterel = Language "Esterel" [discreteSynchronousProgramming]

clu :: Language
clu = Language "CLU" [adtImperativeProgramming]

oCaml :: Language
oCaml = Language "OCaml" [adtImperativeProgramming,
                          sequentialObjectOrientedProgramming]

erlang :: Language
erlang = Language "Erlang" [multiAgentProgramming]

csp :: Language
csp = Language "CSP" [activeObjectProgramming]

occam :: Language
occam = Language "Occam" [activeObjectProgramming]

pascal :: Language
pascal = Language "Pascal" [imperativeProgramming]

c :: Language
c = Language "C" [imperativeProgramming]

dijkstraGCL :: Language
dijkstraGCL = Language "DijkstraGCL" [guardedCommandProgramming]

snobol :: Language
snobol = Language "SNOBOL" [imperativeSearchProgramming]

icon :: Language
icon = Language "Icon" [imperativeSearchProgramming]

java :: Language
java = Language "Java" [concurrentObjectOrientedProgramming]

cplusplus :: Language
cplusplus = Language "C++" [concurrentObjectOrientedProgramming]

smalltalk :: Language
smalltalk = Language "Smalltalk" [concurrentObjectOrientedProgramming]

languages :: [Language]
languages = [xml, 
             sexpression,
             scheme, 
             ml, 
             prolog, 
             clp, 
             life, 
             akl, 
             oz, 
             alice, 
             pcurry, 
             hasKell, 
             pipes, 
             e, 
             fcp, 
             frTime, 
             esterel, 
             clu, 
             oCaml, 
             erlang, 
             csp, 
             occam, 
             pascal, 
             c, 
             dijkstraGCL, 
             snobol, 
             icon, 
             java, 
             cplusplus, 
             smalltalk
            ]
