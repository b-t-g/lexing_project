module Conversion where
import Parse
import qualified Data.Map.Strict as Map

data NFA = Map (Char) ([NFA]) String  Bool | Nil

convert :: Regex -> NFA
convert regex = 
    case regex of
        Or (term) (regex) -> convert_or Nil term regex
        Regex term -> convert_term Nil term

convert_or :: NFA -> Term -> Regex -> NFA
convert_or starting_node term regex =
    undefined

convert_term :: NFA -> Term -> NFA
convert_term starting_node (factor:factors) =
    undefined

kleene_star :: Factor -> NFA
kleene_star (Starred base) =
    undefined

convert_regex :: NFA -> Regex -> NFA
convert_regex starting_node regex =
    case regex of
        Or (regex) (term) -> convert_or starting_node regex term
        Regex term -> convert_term starting_node term

convert_base :: NFA -> Base -> NFA
convert_base starting_node base =
    case base of
        Backslash char -> convert_literal starting_node ("\\"++char:"")
        Base char -> convert_literal starting_node (char:"")
        Parenthesized regex -> convert_regex starting_node regex

convert_literal :: NFA -> String -> NFA
convert_literal starting_node literal = undefined
