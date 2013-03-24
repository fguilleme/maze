module MazeDebug where

import Debug.Trace(trace)

data TRACELEVEL = DEBUG | VERBOSE | SILENT deriving (Enum)

tLevel :: TRACELEVEL
tLevel = SILENT

ztrace :: TRACELEVEL -> String -> a -> a
ztrace level s f | fromEnum tLevel > fromEnum level = f
                 | otherwise = trace s f

