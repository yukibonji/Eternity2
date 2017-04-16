namespace Eternity2

module Basic = 

    let genVar n = 
        let counter = ref n in fun () -> incr counter; !counter

    // Basic type for propositional logic formulas
    type BoolExpr = 
        | Or of BoolExpr * BoolExpr
        | And of BoolExpr * BoolExpr
        | Not of BoolExpr
        | Var of int
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
        Or (Not a, b)

    let (<=>) a b = 
        And (a => b, b => a)

    let var a =
        Var a

    let tt = True
    let ff = False


    // Transformations
    let rec deMorgan expr = 
        match expr with 
        | Not (Not expr') -> expr'

        | Not (Or (first, second)) -> And (Not first, Not second) 
        | Not (And (first, second)) -> Or (Not first, Not second) 

        | And (first, second) -> And (deMorgan first, deMorgan second)
        | Or (first, second) -> Or (deMorgan first, deMorgan second)
        | Not expr' -> Not (deMorgan expr')
        | Var _ | True | False -> expr

    let rec distributive expr = 
        match expr with
        | Or (push, And (first, second)) -> And (Or (push, first), Or (push, second)) 
        | Or (And (first, second), push) -> And (Or (push, first), Or (push, second)) 

        | And (first, second) -> And (distributive first, distributive second)
        | Or (first, second) -> Or (distributive first, distributive second)
        | Not expr' -> Not (distributive expr')
        | Var _ | True | False -> expr



    let rec normalize expr = 
        let expr' = distributive (deMorgan expr)
        if expr = expr' then expr' else normalize expr'