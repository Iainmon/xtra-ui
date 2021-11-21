module Ellipsis exposing (..)
import Html exposing (input)

-- shorten "let fact = λx -> case x of 0 -> 1; y -> y * fact (x - 1); in fact 5" 25 ["=","⇒","of","in"]
-- > "let fact = λx...in fact 5"
--Known issue, firstCut could be negative, temp solved by clamping to at least 0
--Could have longer strings than budget if last token is far from the end
shorten : String -> Int -> List String -> String
shorten input budget tokens = 
    if String.length input < budget
    then input
    else let lastToken = getLastToken input tokens
             cutAmount = (String.length input) - budget + 3
             cutIndex = Tuple.first lastToken
             firstCut = max (cutIndex - cutAmount) 0
        
        in (String.slice 0 firstCut input) ++ "..." ++ String.dropLeft cutIndex input
    

getLastToken : String -> List String -> (Int, String)
getLastToken input tokens = getLastTokenHelper input tokens []

getLastTokenHelper : String -> List String -> List (Int,String) -> (Int, String)
getLastTokenHelper input tokens state =
    case tokens of
        [] -> case state of
                ((a, astr) :: (b, bstr) :: rest) -> 
                    if a > b 
                    then getLastTokenHelper input tokens ((a, astr)::rest)
                    else getLastTokenHelper input tokens ((b, bstr)::rest)
                _ -> case List.head state of
                        Nothing -> (String.length input, "")
                        Just res -> res
        (t::ts) -> 
            case lastLoc t input of
                Just i -> getLastTokenHelper input ts ((i, t)::state)
                Nothing -> getLastTokenHelper input ts (state)


lastLoc : String -> String -> Maybe Int
lastLoc token input =
    case String.indexes token input of
        [] -> Nothing
        inds -> List.head <| List.reverse inds 