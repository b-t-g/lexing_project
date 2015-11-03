module Conversion where
import Parse
import qualified Data.Map.Strict as Map

data NFA = Map (Char) ([NFA]) String | Nil

convert :: Regex -> NFA
convert regex = 
    case regex of
        Or (regex1) (regex2) -> convert_or regex1 regex2
        Regex term -> convert_term term
        (x:xs) -> convert_term (x:xs)
        Starred base -> kleene_stare base
        Parenthesized regex -> convert_regex regex
        Unstarred base -> convert_literal base
        Base char -> convert_literal char
        Backslash char -> convert_literal char

convert_or :: Regex -> NFA
convert_or regex = undefined

