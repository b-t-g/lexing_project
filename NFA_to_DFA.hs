module NFA_to_DFA where
import Conversion
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data DFA = Node {
    transitions  :: Map.Map [NFA] (Map.Map (Char) ([NFA]))
  , final_state  :: Bool
    } deriving (Ord, Eq)

nfa_to_dfa :: [NFA] -> DFA -> DFA
nfa_to_dfa nfa dfa =
    let starting_node_list = remove_empty_transitions nfa in
        let new_map = combine_nfa_list nfa in
            if elem starting_node_list (Map.keys $ NFA_to_DFA.transitions dfa)
                then dfa
            else
                let new_dfa = NFA_to_DFA.Node (Map.insert starting_node_list (combine_nfa_list nfa)
                                              (NFA_to_DFA.transitions dfa))
                              (or $ map (\x -> Conversion.final_state x) starting_node_list) in
                    let new_transitions = Map.findWithDefault Map.empty starting_node_list
                                          (NFA_to_DFA.transitions new_dfa) in
                        foldr (\x node -> nfa_to_dfa (Map.findWithDefault [] x new_transitions)
                                          node)
                        new_dfa (Map.keys new_transitions)


--Takes a list of nfas and returns a map by combining the transitions of the nfa and removing epsilon transitions
combine_nfa_list :: [NFA] -> Map.Map Char [NFA]
combine_nfa_list nfas =
    foldr (\x -> combine_maps x) (Map.empty::(Map.Map Char [NFA])) nfas 
    
combine_maps :: NFA -> Map.Map Char [NFA] -> Map.Map Char [NFA]
combine_maps nfa map =
    let nfa_map = Conversion.transitions nfa in
        let keys = convert_to_char (Map.keys nfa_map) in
            if keys == []
                then map
            else
                foldr (\x trans -> (combine_maps (nfa)
                                   (Map.insert (x) (Map.findWithDefault [] (Conversion.Transition x)
                                    nfa_map) trans))) map keys

convert_to_char :: [Transition] -> [Char]
convert_to_char transitions =
    if transitions == []
        then []
    else
    case head transitions of
        Epsilon -> convert_to_char (tail transitions)
        Transition char -> char : convert_to_char (tail transitions)
        Escaped char -> '\\' : char : convert_to_char (tail transitions)

remove_empty_transitions :: [NFA] -> [NFA]
remove_empty_transitions list =
    case list of
    (x:xs) -> x : (remove_empty_transitions (xs++
                  ((dropWhile (\x -> elem x list))
                  (Map.findWithDefault [] Epsilon (Conversion.transitions x)))))
    [] -> []
