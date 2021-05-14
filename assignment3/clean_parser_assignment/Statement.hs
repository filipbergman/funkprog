module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Read String |
    Write Expr.T |
    Begin [Statement] |
    While Expr.T Statement |
    Comment String
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifState = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
buildIf (e, s) = If (fst e) (snd e) s

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip (s) = Skip

readState = accept "read" -# word #- require ";" >-> buildRead
buildRead (s) = Read s

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite (e) = Write e

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin (s) = Begin s

while = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile
buildWhile (e, s) = While e s

comment = accept "--" #- require "\n" >-> buildComment
buildComment (s) = Comment s

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment str expr: stmts) dict input = 
    exec stmts (Dictionary.insert (str, Expr.value expr dict) dict) input
exec (Skip:stmts) dict input =
    exec stmts dict input
exec (Read str:stmts) dict (x:xs) =
    exec stmts (Dictionary.insert (str, x) dict) xs
exec (Begin x:stmts) dict input = 
    exec (x++stmts) dict input
exec (While expr st:stmts) dict input = 
    if(Expr.value expr dict) > 0
    then exec (st:stmts) dict input
    else exec stmts dict input
exec (Write expr:stmts) dict input =
    (Expr.value expr dict):(exec stmts dict input)
exec (Comment str:stmts) dict input = 
    exec stmts dict input


instance Parse Statement where
  parse = readState ! skip ! write ! assignment ! ifState ! while ! begin ! comment
  toString = printStatement

printStatement :: Statement -> String
printStatement (Assignment str e) = str ++ " := " ++ Expr.toString e ++ ";\n"
printStatement (Skip) = "skip;\n"
printStatement (Read s) = "read " ++ s ++";\n"
printStatement (Write e) = "write " ++ Expr.toString e ++ ";\n"
printStatement (Begin xs) = "begin\n"
printStatement (While e st) = "while " ++ Expr.toString e ++ "do\n" ++ printStatement st
printStatement (If x s1 s2) = ""
printStatement (Comment s) = "--" ++ s