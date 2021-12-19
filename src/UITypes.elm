module UITypes exposing(..)

type alias InitType = 
    { actions : List String
    , funcFormat : FunctionFormat
    , filters : List (String,Bool)
    , shortenTokens : List String
    }

type FilterInput
    = Filter String
    | Param String

type FunctionFormat
    = FunctionFormat String String String -- StartToken, EqToken, EndToken
    | NoFormat String

functionToString : FunctionFormat -> (String, String) -> String
functionToString format (name, body) = case format of
   FunctionFormat start eq end  -> String.join " " [start, name, eq, body, end]
   NoFormat eq -> String.join " " [name, eq, body]

filterToString : String -> FilterInput -> Maybe String -> Maybe String
filterToString action filter input = case filter of
    Filter filt -> case input of
        Nothing -> Just (String.join " " [action,filt])  --filter with no param
        _ -> Nothing    --given param it doesn't need
    Param filt -> case input of
        Just param -> Just (String.join " " [action, filt, param]) --filter with param
        _ -> Nothing --needs param, not given


initTypeToString : InitType -> String
initTypeToString data =  
    let aStr = (String.join " " data.actions)
        bStr = (functionToString data.funcFormat ("",""))
        cStr = (String.join " " <| List.map Tuple.first data.filters)
        eStr = (String.join " " data.shortenTokens)
    in String.join "\n" [aStr, bStr, cStr, eStr]