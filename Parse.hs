module Parse (parse) where

import Data.Char
import Data.Ord
import Data.List
import Data.Function
import Data.Functor
import Data.Bifunctor
import Control.Applicative
import Control.Monad
import Text.ParserCombinators.ReadP
import ReadPPlus
import DataStructure


---- Run Parser ----

-- | Run the parser on a given string.
parse :: String -> Either String MathExp
parse str =
    case (completeParses, incompleteParses) of
        ([(result, "")], _  ) -> Right result  -- Only complete result.
        ([]            , [] ) -> Left   "No parse."
        ([]            , _:_) -> Left $ "Incomplete parse. Unparsed: " ++ show leastRemaining
        (_:_           , _  ) -> Left $ "Ambiguous parse: " ++ show (map (first show) completeParses)
    where
        parses = readP_to_S (parseMathExp <* skipSpaces) str
        (completeParses, incompleteParses) =
            partition (\(_, remaining) -> remaining == "") parses
        leastRemaining = minimumBy (comparing length) . map snd $ incompleteParses


---- MathExp ----

parseMathExp :: ReadP MathExp
parseMathExp = infixExpr finalLevel opArith where
    opArith = zip3 assocArith opArithUnary opArithBinary
    finalLevel = Val <$> parseExps

-- parse continuous preorder functions
parseExps :: ReadP Application
parseExps = foldl1 Apply <$> (map Ground <$> some parseExp)

-- | parse what lies between operators
parseExp :: ReadP MathExp
parseExp = exprNum <++ exprVar <++ exprLet <++ exprIf <++ exprLambda <++ exprParen
    -- not exprCond, which is only used in "if" and "let"

exprNum, exprVar, exprLet, exprCond, exprIf, exprLambda, exprParen :: ReadP MathExp
exprNum = Number <$> parseNumber
exprVar = Var <$> parseName
exprLet = Let <$> parseVars <*> parseVals <*> parseUsage <* endOfExpr
exprCond = Cond <$> parseCondition
exprIf = If <$> parseIf <*> parseThen <*> parseElse <* endOfExpr
exprLambda = pureChar '\\' *> parseLambda <* endOfExpr
exprParen = pureParenthesis parseMathExp


---- lambda expression ----

parseLambda :: ReadP MathExp
parseLambda = Lambda <$> parseName <*> parseFunc <++ parseLambda where
    parseFunc = pureString "->" *> parseMathExp


---- let in ----

parseVars :: ReadP [Name]
parseVars = pureString "let" *> possibleParenCommaList parseName

parseVals :: ReadP [MathExp]
parseVals = pureChar '=' *> possibleParenCommaList (exprCond <++ parseMathExp)
    -- exprCond must come before parseMathExp because parseMathExp is contained in exprCond

parseUsage :: ReadP MathExp
parseUsage = pureString "in" *> parseMathExp

-- | lest the parentheses of inner expression be misconsumed
possibleParenCommaList :: ReadP a -> ReadP [a]
possibleParenCommaList parser =
        ((:[]) <$> parser)
    <++ pureParenthesis (sepBy1 parser (pureChar ','))


---- if then else ----

parseIf, parseThen, parseElse :: ReadP MathExp
parseIf = pureString "if" *> (exprCond <++ exprVar)
parseThen = pureString "then" *> parseMathExp
parseElse = pureString "else" *> parseMathExp

parseCondition :: ReadP Condition
parseCondition = infixExpr finalLevel opLogic where
    opLogic = zip3 assocLogic opLogicUnary opLogicBinary
    finalLevel = parseBoolean

parseBoolean :: ReadP Condition
parseBoolean = parseTrue <++ parseFalse <++ parseCompare <++ parseLogicParen where
    parseTrue = pureString "True" $> Boolean True
    parseFalse = pureString "False" $> Boolean False
    parseCompare = (&) <$> parseMathExp <*> pureTryKeys comparators <*> parseMathExp
    parseLogicParen = pureParenthesis parseCondition


---- basic data types ----

parseNumber :: ReadP Number
parseNumber = read <$> pureMunch1 isDigit

parseName :: ReadP Name
parseName = do
    res <- (:) <$> pureSatisfy isLower <*> munch isAlphaNum
    guard (res `notElem` illegalNames)
    return res

illegalNames :: [Name]
illegalNames = ["let", "in", "if", "then", "else", "not"]

pureParenthesis :: ReadP a -> ReadP a
pureParenthesis = between (pureChar '(') (pureChar ')')

endOfExpr :: ReadP ()
endOfExpr =
        pureEOF
    <++ pureFollowedByChar ')'
    <++ pureFollowedByChar ','
    <++ branch pureFollowedByString illegalNames
    <++ branch pureFollowedByString (map fst comparators)
    <++ branch pureFollowedByString (map fst $ concat opLogicBinary)


---- parse values seperated by unary and binary operators of different precedences ----

-- | requires a list of unary operators, binary operators, 
--   and associativity of each level.
--   also requires a final level resolution
infixExpr ::  ReadP a -> [(Associativity, [(String, Unary a)], [(String, Binary a)])] -> ReadP a
infixExpr finalLevel [] = finalLevel
infixExpr finalLevel ((assoc, opU, opBi):opLs) =
    prefix nextLevel unaries `chain1Func` binaries where
        prefix :: ReadP a -> ReadP (Unary a) -> ReadP a -- parse a value probably preceded by a unary operator
        prefix parser opU = fix $ (<++) parser . (<*>) opU
        nextLevel = infixExpr finalLevel opLs
        unaries = pureTryKeys opU
        binaries = pureTryKeys opBi
        chain1Func = case assoc of
            LeftAssoc -> chainl1
            RightAssoc -> chainr1