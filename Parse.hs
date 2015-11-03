module Parse (
parse_regex,
Regex,
Term,
Factor,
Base
) where
data Regex = Or Term Regex | Regex Term deriving(Eq)

type Term = [Factor]

data Factor = Unstarred Base | Starred Base deriving(Eq)

data Base = Base Char | Backslash Char | Parenthesized Regex deriving(Eq)

instance Show Regex where
    show (Or term regex) = "(Or " ++ show term ++ " " ++ show regex ++ ")"
    show (Regex term) = show term

instance Show Factor where
    show (Unstarred base) = show base
    show (Starred base) = show "(kleene-star " ++ show base ++ " )"

instance Show Base where
    show (Base char) = show char
    show (Backslash char) = "\\" ++ show char
    show (Parenthesized regex) = "( " ++ show regex ++ " )"
        
eat :: String -> Char -> String
eat regex expected = 
    if regex == ""
        then ""
    else if (head regex) == expected
        then tail regex
    else ""

peek::String -> Char
peek string =
    if string == ""
        then ' '
    else head string

parse_regex :: String -> (Regex, String)
parse_regex regex = 
    let term = parse_term regex [] in
        if (snd term == "")
            then (Regex $ fst term, snd term)
        else if (head $ snd term) == '|'
            then let or_regex = eat (snd term) ('|') in
                parse_or (or_regex) (fst term)
        else (Regex $ fst term, snd term)

parse_or :: String -> Term -> (Regex, String)
parse_or remaining regex =
    let new_regex = parse_regex remaining in
        (Or regex (fst $ new_regex), snd $ new_regex)

parse_term :: String -> Term -> (Term, String)
parse_term unparsed_term found_terms =
     if (unparsed_term == "" || (head unparsed_term == '|') ||
        (head unparsed_term == ')'))
        then (found_terms, unparsed_term)
    else let factor = parse_factor unparsed_term in
        parse_term (snd factor) (found_terms ++ [fst factor]) 

multiple_factors :: Term -> String -> (Term, String)
multiple_factors partial_factor term = 
    let factor = parse_factor term in
    if (fst factor) == Unstarred (Base ' ')
        then (partial_factor, "")
    else if (head term == '|') || (head term == ')')
        then (partial_factor, term)
    else (multiple_factors (partial_factor ++ [fst factor]) (snd factor))
    
    
parse_factor :: String -> (Factor, String) 
parse_factor factor = 
    let base = parse_base factor in
        if peek (snd base) == '*'
            then (Starred (fst base), eat (snd base) '*')
        else (Unstarred (fst base), snd base)


parse_base :: String -> (Base, String)
parse_base base = 
    if base == ""
        then (Base ' ',"")
    else if head base == '\\'
            then let eaten_base = eat base '\\' in
                (Backslash $ peek eaten_base, eat eaten_base $ peek eaten_base)
        else if head base == '('
            then (Parenthesized (fst (parse_regex (tail base))),
                  eat (snd (parse_regex (tail base))) ')')
            else (Base (head base), tail base)
