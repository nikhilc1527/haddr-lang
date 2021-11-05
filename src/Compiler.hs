module Compiler where

import Data.Map as Map

type ASMText = [Either String (String, [String])]

-- compileSource :: String
-- compileSource = undefined

type Symbol = (TypeID, String, Int)
type SymbolTable = Map.Map Expression 

compileASM :: ASM -> String
compileASM = undefined

exprToASM :: Expression -> ASM
exprToASM = undefined
