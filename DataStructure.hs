module DataStructure where

import Data.List ( intercalate )

---- Application ----

-- deals with a series of preorder operations seperated by space
data Application
    = Ground MathExp
    | Apply Application Application

instance Show Application where show = unparseApp
unparseApp :: Application -> String
unparseApp (Ground expr) = unparseExpr expr
unparseApp (Apply app1 app2) = "(" ++ unparseApp app1 ++ " " ++ unparseApp app2 ++ ")"


---- MathExp ----

type Name   = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.

data MathExp
    = Number Number
    | Var Name
    | Val Application
    | Let [Name] [MathExp] MathExp
    | Lambda Name MathExp
    | Cond Condition
    | If MathExp MathExp MathExp
    | Neg MathExp
    | Plus MathExp MathExp
    | Minus MathExp MathExp
    | Mult MathExp MathExp
    | Div MathExp MathExp
    | Pow MathExp MathExp

instance Show MathExp where show = unparseExpr
unparseExpr :: MathExp -> String
unparseExpr (Number n) = show n
unparseExpr (Var name) = name
unparseExpr (Val app) = unparseApp app
unparseExpr (Let vars vals expr) = "(let " ++ commaList vars ++ " = " ++ commaList (map unparseExpr vals) ++ " in " ++ unparseExpr expr ++ ")"
    where
    commaList :: [String] -> String
    commaList [] = "No Item!"
    commaList [str] = str
    commaList strs = "(" ++ intercalate ", " strs ++ ")"
unparseExpr (Lambda arg func) = "(\\" ++ arg ++ " -> " ++ unparseExpr func ++ ")"
unparseExpr (Cond cond) = unparseCond cond
unparseExpr (If cond expr1 expr2) = "(if " ++ unparseExpr cond ++ " then " ++ unparseExpr expr1 ++ " else " ++ unparseExpr expr2 ++ ")"
unparseExpr (Neg   expr) = "- " ++ unparseExpr expr
unparseExpr (Plus  expr1 expr2) = "(" ++ unparseExpr expr1 ++ " + " ++ unparseExpr expr2 ++ ")"
unparseExpr (Minus expr1 expr2) = "(" ++ unparseExpr expr1 ++ " - " ++ unparseExpr expr2 ++ ")"
unparseExpr (Mult  expr1 expr2) = "(" ++ unparseExpr expr1 ++ " * " ++ unparseExpr expr2 ++ ")"
unparseExpr (Div   expr1 expr2) = "(" ++ unparseExpr expr1 ++ " / " ++ unparseExpr expr2 ++ ")"
unparseExpr (Pow   expr1 expr2) = "(" ++ unparseExpr expr1 ++ " ^ " ++ unparseExpr expr2 ++ ")"

---- Associativity ----

data Associativity = LeftAssoc | RightAssoc


---- operators ----

type Unary a = a -> a
type Binary a = a -> a -> a

-- | recognizable arithmetic unary operators
opArithUnary :: [[(String, Unary MathExp)]]
opArithUnary =
    [   [                   ]     -- level 1
    ,   [   ("-", Neg)      ]     -- level 2
    ,   [                   ]   ] -- level 3

-- | recognizable arithmetic binary operators
opArithBinary :: [[(String, Binary MathExp)]]
opArithBinary = 
    [   [   ("+", Plus )          -- level 1
        ,   ("-", Minus)    ]
    ,   [   ("*", Mult )          -- level 2
        ,   ("/", Div  )    ]
    ,   [   ("^", Pow  )    ]   ] -- level 3

assocArith :: [Associativity]
assocArith = [LeftAssoc, LeftAssoc, RightAssoc]


---- Condition ----

data Condition
    = Boolean Bool
    | Not Condition
    | And Condition Condition
    | Or  Condition Condition
    | E  MathExp MathExp -- equal to
    | NE MathExp MathExp -- not equal to
    | G  MathExp MathExp -- greater than
    | L  MathExp MathExp -- less than
    | GE MathExp MathExp -- greater than or equal to
    | LE MathExp MathExp -- less than or equal to

instance Show Condition where show = unparseCond
unparseCond :: Condition -> String
unparseCond (Boolean bool) = show bool
unparseCond (Not cond) = "not " ++ unparseCond cond
unparseCond (And cond1 cond2) = "(" ++ unparseCond cond1 ++ "&&" ++ unparseCond cond2 ++ ")"
unparseCond (Or  cond1 cond2) = "(" ++ unparseCond cond1 ++ "||" ++ unparseCond cond2 ++ ")"
unparseCond (E   expr1 expr2) = unparseExpr expr1 ++ "==" ++ unparseExpr expr2
unparseCond (NE  expr1 expr2) = unparseExpr expr1 ++ "/=" ++ unparseExpr expr2
unparseCond (G   expr1 expr2) = unparseExpr expr1 ++ ">"  ++ unparseExpr expr2
unparseCond (L   expr1 expr2) = unparseExpr expr1 ++ "<"  ++ unparseExpr expr2
unparseCond (GE  expr1 expr2) = unparseExpr expr1 ++ ">=" ++ unparseExpr expr2
unparseCond (LE  expr1 expr2) = unparseExpr expr1 ++ "<=" ++ unparseExpr expr2

-- it is important to have >= comes before > and <= comes before <,
-- given the nature of PureParse.branchParse.
comparators :: [(String, MathExp -> MathExp -> Condition)]
comparators =   [ ("==" , E  )
                , ("/=" , NE )
                , (">=" , GE )
                , ("<=" , LE )
                , (">"  , G  )
                , ("<"  , L  ) ]

-- | recognizable logic unary operators
opLogicUnary :: [[(String, Unary Condition)]]
opLogicUnary =
    [   [                   ]     -- level 1
    ,   [   ("not", Not)    ]   ] -- level 2

-- | recognizable logic binary operators
opLogicBinary :: [[(String, Binary Condition)]]
opLogicBinary =
    [   [   ("&&", And)     ]     -- level 1
    ,   [   ("||", Or )     ]   ] -- level 2

assocLogic :: [Associativity]
assocLogic = [LeftAssoc, LeftAssoc]