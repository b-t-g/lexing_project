module Conversion where
import Parse
import qualified Data.Map.Strict as Map

--The empty string is not a valid character hence the necessity of the epsilon
data Transition = Transition Char | Epsilon | Escaped Char deriving (Ord, Eq)

instance Show Transition where
    show (Transition char) = show char
    show (Epsilon) = "Epsilon"
    show (Escaped char) = '\\':(show char)

{-
An NFA is a map from characters to a list of NFA's and a boolean which
states whether the node is an ending state
-}

data NFA = Node {
    transitions :: (Map.Map (Transition) ([NFA]))
  , final_state :: Bool
} deriving (Ord, Eq)


convert :: Regex -> NFA
convert regex = 
    case regex of
        Or (term) (regex) -> convert_or term regex (Node (Map.empty) False)
        Regex term -> convert_term term ((Node (Map.empty) False))

convert_or :: Term -> Regex -> NFA -> NFA
convert_or term regex ending_node=
    let term_ending_node = Node (Map.singleton Epsilon [ending_node]) False in
        let term_initial_node = convert_term term term_ending_node in
            let regex_ending_node = Node (Map.singleton Epsilon [ending_node]) False in
                let regex_initial_node = convert_regex regex regex_ending_node in
                    Node (Map.fromList [(Epsilon, [term_initial_node, regex_initial_node])]) False
                

convert_term :: Term -> NFA -> NFA
convert_term term ending_node =
    case term of
        (x:xs) -> let factor_NFA = convert_factor (last term) ending_node in
                      convert_term (init term) factor_NFA
        [] -> ending_node

convert_factor :: Factor -> NFA -> NFA
convert_factor factor ending_node =
    case factor of
        (Unstarred base) -> convert_base base ending_node
        (Starred base) -> kleene_star base ending_node

kleene_star :: Base -> NFA -> NFA
kleene_star base ending_node =
    let penultimate_node = Node (Map.fromList [(Epsilon,
    [ending_node, (convert_base base penultimate_node)])]) False in
        let base_node = convert_base base penultimate_node in
            Node (Map.fromList [(Epsilon, [base_node,ending_node])]) False
        

convert_regex :: Regex -> NFA -> NFA
convert_regex regex ending_node =
    case regex of
        Or (regex) (term) -> convert_or regex term ending_node
        Regex term -> convert_term term ending_node

convert_base :: Base -> NFA -> NFA
convert_base base ending_node =
    case base of
        Backslash char -> convert_literal (Escaped char) ending_node
        Base char -> convert_literal (Transition char) ending_node
        Parenthesized regex -> convert_regex regex ending_node

convert_literal :: Transition -> NFA -> NFA
convert_literal literal ending_node = 
    case literal of
        Escaped char -> convert_escaped_character char ending_node
        Transition char -> convert_character char ending_node

convert_character :: Char -> NFA -> NFA
convert_character char ending_node =
    --let ending_node = Node (Map.empty) False in
     Node (Map.singleton (Transition char) [ending_node]) True

convert_escaped_character :: Char -> NFA -> NFA
convert_escaped_character char ending_node = 
    --let ending_node = Node (Map.empty) False in
        let penultimate_node = Node (Map.singleton (Transition char) [ending_node]) True in
            Node (Map.singleton (Transition '\\') [penultimate_node]) True 
