import Data.List (isSuffixOf)

type Delta = Int -> Char -> Int

-- get_delta takes a pattern - a string
-- get_delta returns a function that takes current_state (length of current match) and a letter
get_delta :: String -> Delta
get_delta pattern = \current_state letter ->
    let current_match = take current_state pattern
    -- take maximal length of matched pattern for current letter added to the match
    in head [k | k <- [current_state+1, current_state .. 0],
                (take k pattern) `isSuffixOf` (current_match ++ [letter])]

finite_automata :: String -> String -> [Int]
finite_automata pattern text =
    -- go function takes current state, remaining text and all found matches
    let go state [] matches = matches
        -- check if the new letter creates a new match
        go state (letter:rest) matches =
            let new_state = delta state letter
            in if new_state == len
               then go new_state rest ((full_len - len - length rest) : matches)
               else go new_state rest matches
    in reverse (go 0 text [])
    where
        full_len = length text
        len = length pattern
        delta = get_delta pattern
