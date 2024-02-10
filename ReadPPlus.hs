module ReadPPlus where
import Text.ParserCombinators.ReadP
import Data.Functor
import Control.Monad ( when )


---- followed by ----

-- | check if followed by a parser, without consuming the following
followedBy :: ReadP a -> ReadP ()
followedBy p = do
    rest <- look
    let isFollowedby = null $ readP_to_S p rest
    when isFollowedby pfail


---- parse with key ----

---- apply a parser to a list of values, and combine them with choice
branch :: (a -> ReadP b) -> [a] -> ReadP b
branch parser = choice . map parser

-- | provided a (key,value) pair and an verifying parser, return the parse result
applyKey :: (String, a) -> ReadP a
applyKey (k,v) = string k $> v

-- | provided a list of (key,value) pairs and an verifying parser, return the non-fail results
tryKeys :: [(String, a)] -> ReadP a
tryKeys = branch applyKey


---- parse and skip spaces ----

-- | eliminate all spaces al the beginning, then apply parser
purify :: ReadP a -> ReadP a
purify = (skipSpaces *>)

pureEOF :: ReadP ()
pureEOF = purify eof

pureSatisfy :: (Char -> Bool) -> ReadP Char
pureSatisfy = purify . satisfy

pureChar :: Char -> ReadP Char
pureChar = purify . char

pureString :: String -> ReadP String
pureString = purify . string

pureMunch1 :: (Char -> Bool) -> ReadP String
pureMunch1 = purify . munch1

pureFollowedBy :: ReadP a -> ReadP ()
pureFollowedBy = purify . followedBy

pureFollowedByChar :: Char -> ReadP ()
pureFollowedByChar = pureFollowedBy . char

pureFollowedByString :: String -> ReadP ()
pureFollowedByString = pureFollowedBy . string

pureTryKeys :: [(String, a)] -> ReadP a
pureTryKeys = purify . tryKeys