module CustomDecoders exposing (..)

import Json.Decode exposing (Decoder, field, list, string, map3, map4, index, succeed, bool)
import Json.Decode.Extra as DE
import UITypes exposing (InitType,FunctionFormat(..))

actionsDecoder : Decoder (List String)
actionsDecoder = field "actions" (list string)

funcFormatDecoder : Decoder (FunctionFormat)
funcFormatDecoder = field "funcFormat" functionFormatDecoder

filtersDecoder : Decoder (List (String,Bool))
filtersDecoder = field "filters" (list filterDecoder)
filterDecoder : Decoder (String,Bool)
filterDecoder = 
    succeed Tuple.pair
        |> DE.andMap (field "name" string)
        |> DE.andMap (field "param" bool)


shortenTokensDecoder : Decoder (List String)
shortenTokensDecoder = field "shortenTokens" (list string)

functionFormatDecoder : Decoder (FunctionFormat)
functionFormatDecoder = map3 FunctionFormat (index 0 string) (index 1 string) (index 2 string)


initDecoder : Decoder (InitType)
initDecoder = map4 InitType actionsDecoder funcFormatDecoder filtersDecoder shortenTokensDecoder