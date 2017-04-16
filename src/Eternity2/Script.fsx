#time

// Basic type for propositional logic formulas
type BoolExpr = 
    | Or of BoolExpr * BoolExpr
    | And of BoolExpr * BoolExpr
    | Not of BoolExpr
    | Var of string
    | True 
    | False

// Basic operators
let (<&>) a b =
    And (a, b)

let (<|>) a b =
    Or (a, b)

let (~~) a = 
    Not a 

let (=>) a b =
    Or (a, Not b)

let (<=>) a b = 
    And (a => b, b => a)

let var a =
    Var a

let tt = True
let ff = False


