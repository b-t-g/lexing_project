module NFA_to_DFA where
import Conversion
import qualified Data.Map.Strict as Map

data DFA = Node {
    transitions  :: (Map.Map (Char) (DFA))
  , final_state  :: Bool
    } deriving (Ord, Eq)

empty_transitions :: [NFA] -> [NFA]
empty_transitions current_list =
    case current_list of
        (x:xs) -> let head_transitions = Conversion.transitions $ head current_list in
                      Map.findWithDefault [] Epsilon head_transitions ++ empty_transitions (tail current_list)
        [] -> [] 

