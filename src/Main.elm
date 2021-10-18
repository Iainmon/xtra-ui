module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Draggable exposing (Msg)
import Html.Attributes exposing (rows)
import Html.Attributes exposing (cols)
import Http
import Json.Decode as D
import Json.Encode as JE
import Draggable exposing (init)
import SvgParser exposing(parse)
import String exposing(split)
import Html.Attributes exposing (height)
import Html.Attributes exposing (width)
import Html.Attributes exposing (style)
import ElmEscapeHtml exposing (unescape)
import Regex

type alias Model = { program : String, filter : String, dotString : String, image : String}

init : () -> (Model, Cmd Msg)
init _ = 
    ( { program = "", filter = "", dotString = "", image = ""}
    , Cmd.none
    )

exampleProg : String
exampleProg = "let fact = \\x -> case x of 0 -> 1; y -> y * fact (x - 1); in fact 5"

exampleFilter : String
exampleFilter = "recursive s = (nonFirstTwo s) except (afterLast s)\n\ndecBy1 = <_X - 1 => _Y>\nintermediateFact = recursive <fact _X => _Y>\ncases = <fact _X => _Y> then children\n\nhide <let fact = _X in _Y => _Z>\nhide cases\nfactor binding\nfactor decBy1 then all\nhide pattern\nhide intermediateFact\nhide reflexive"

krokiURL : String
krokiURL = "http://localhost:8001/graphviz/svg"

xtraBackendURL : String
xtraBackendURL = "http://localhost:8081/trace"

view : Model -> Html Msg
view model =
    table [] 
        [tr [] 
            [th [] [text "Input"]
            ,th [] [text "Output"]
            ]
        , tr []
            [td [ style "vertical-align" "top" ] 
                [button [onClick Clear] [text "Clear"]
                ,button [onClick Run] [text "Run"]
                ,button [onClick LoadEx] [text "LoadEx"]
                ,hr [] []
                ,textarea [rows 8, cols 60, onInput SaveProg] [text <| .program model]
                ,hr [] []
                ,textarea [rows 8, cols 60, onInput SaveFilt] [text <| .filter model]
                ,hr [] []
                ,textarea [rows 8, cols 60] [text <| (.image model) ]
                ,hr [] []
                ,textarea [rows 8, cols 60, onInput ManualDot ] [text <| (.dotString model) ]
                ]
            ,td [] [ div [height 600, width 400] [svgOrNot model] ]
            ]
        ]


type Msg 
    = Run
    | Clear
    | LoadEx 
    | SaveProg String
    | SaveFilt String
    | GotDot (Result Http.Error String)
    | GotSvg (Result Http.Error String)
    | ManualDot String

svgOrNot : Model -> Html Msg
svgOrNot model = 
    if .image model == "" then
        div [] []
    else
        case parse (cleanSvgString (.image model)) of
            Err msg -> div [] [text msg]
            Ok elem -> elem

cleanSvgString : String -> String
cleanSvgString str 
    = String.dropLeft 222 str
    {--let res = List.head ( List.drop 1 (String.split "<!-- Pages: 1 --> " str)) in
        case res of
            Just svg -> svg
            Nothing -> "Parsing Error"
           --}



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Clear ->
            init ()
        LoadEx ->
            update Run {model | program = exampleProg, filter = exampleFilter, image = ""}
        SaveProg prog ->
            update Run {model | program = prog}
        SaveFilt filt ->
            update Run {model | filter = filt}
        Run ->
            ( model, getDotString model )
        GotDot (Ok dot) ->
            ( {model | dotString = fixNodes dot}, getSvg {model | dotString = fixNodes dot} )
        GotDot (Err httpError) ->
            ( { model | dotString = buildErrorMessage httpError}
            , Cmd.none
            )
        GotSvg (Ok svg) ->
            ( {model | image = stringFindAndReplace svg svgReplace}, Cmd.none)--( {model | image = unescape svg}, Cmd.none)
        GotSvg (Err httpError) ->
            ( { model | image = buildErrorMessage httpError}
            , Cmd.none
            )
        ManualDot dot ->
            ( {model | dotString = dot}, getSvg {model | dotString = dot} )


getDotString : Model -> Cmd Msg
getDotString model = 
{--}
    Http.post
    { url = xtraBackendURL
        , expect = Http.expectJson GotDot (D.field "dot" D.string)
        , body = Http.jsonBody <| JE.object [ ("prog", JE.string (.program model)), ("filter", JE.string (.filter model))]
    }
--}
{--
    Http.request
    {   url = xtraBackendURL
        , method = "GET"
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson GotDot (D.field "dot" D.string)
        , body = Http.jsonBody <| JE.object [ ("prog", JE.string (.program model)), ("filter", JE.string (.filter model))]
    }
--}

getSvg : Model -> Cmd Msg
getSvg model =
    Http.post
    { url = krokiURL
        , expect = Http.expectString GotSvg
        , body = Http.jsonBody <| JE.object [ ("diagram_source", JE.string (.dotString model)), ("layout", JE.string "sfdp") ]
    }

buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message

nodePattern = "\">[0-9].</FONT>"

--nodeTest = "<FONT COLOR=\"#0000ff\" POINT-SIZE=\"9\">66</FONT>\n<FONT COLOR=\"#0000ff\" POINT-SIZE=\"9\">65</FONT>"
beforeNode = "POINT-SIZE=\"9\">"
afterNode = "</FONT>"

fixNodes : String -> String
fixNodes dot = 
    let nodeNums = findAllNodeNumbers dot in
    let alphas = List.reverse (getAlphaNodes (List.length nodeNums)) in
    let replacers = genReplaceNode nodeNums alphas in
    stringFindAndReplace dot replacers


genReplaceNode : List Int -> List String -> List (String, String)
genReplaceNode num alpha =
    case (num,alpha) of
       ([],_) -> []
       (_,[]) -> []
       ((n :: ns),(a :: alphs)) -> [(wrapNodeMatch (String.fromInt n), wrapNodeMatch a)] ++ genReplaceNode ns alphs

wrapNodeMatch : String -> String
wrapNodeMatch i = beforeNode ++ i ++ afterNode

findAllNodeNumbers : String -> List Int
findAllNodeNumbers dot = pullIDs (Regex.find nodeRegex dot)

--genReplacementList : List Int -> 

pullIDs : List Regex.Match -> List Int
pullIDs list =
    case list of
        [] -> []
        (x :: xs) -> 
            let y = .match x in
                [Maybe.withDefault 0 (String.toInt (String.slice 2 -7 y))] ++ pullIDs xs

--"[0-9]. -> [0-9].;"
--"<FONT COLOR="#0000ff" POINT-SIZE="9">[0-9].<\/FONT>"

nodeRegex : Regex.Regex
nodeRegex = Maybe.withDefault Regex.never (Regex.fromString nodePattern)


-- TODO: This kinda works, boundaries are wonky, but ok for now. fix later.
getAlphaNodes : Int -> List String
getAlphaNodes i =
    if i < 1 then
        []
    else 
        getAlphaNodes (i - 1) ++ [getNodeExact i]

getNodeExact : Int -> String
getNodeExact i =
    if i == 0 then
        ""
    else if i <= 26 then
        String.fromChar (Char.fromCode (i + 64))
    else 
        let iDiv = i // 26 in
        let iRem = remainderBy 26 i in 
        (getNodeExact iDiv) ++ (getNodeExact (iRem))


--Source -> Replacements -> Output
stringFindAndReplace : String -> List (String,String) -> String
stringFindAndReplace source list =
    case list of
       [] -> source
       ((match, replacement) :: xs) -> stringFindAndReplace (String.replace match replacement source) xs

svgReplace : List (String,String)
svgReplace =    [("&quot;","\"")
                ,("&#45;","-")
                ,("&#32;"," ")
                ,("&#160;"," ")]



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
