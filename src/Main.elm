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

type alias Model = { program : String, filter : String, dotString : String, image : String}

init : () -> (Model, Cmd Msg)
init _ = 
    ( { program = "", filter = "", dotString = "", image = ""}
    , Cmd.none
    )

exampleProg : String
exampleProg = "let fact = \\x -> case x of 0 -> 1; y -> y * fact (x - 1); in fact 5"

exampleFilter : String
exampleFilter = "hide <let fact = _X in _Y => _Z>\nhide <fact _X => _Y> then children\nfactor binding\nfactor <_X - 1 => _Y> then all\nhide pattern\nhide (nonFirstTwo <fact _X => _Y>) except (afterLast <fact _X => _Y>)\nhide reflexive"

krokiURL : String
krokiURL = "http://localhost:8001/graphviz/svg"

xtraBackendURL : String
xtraBackendURL = "http://localhost:8081/trace"

view : Model -> Html Msg
view model =
    div []
        [textarea [rows 8, cols 60, onInput SaveProg] [text <| .program model]
        ,hr [] []
        ,textarea [rows 8, cols 60, onInput SaveFilt] [text <| .filter model]
        ,hr [] []
        ,button [onClick Clear] [text "Clear"]
        ,button [onClick Run] [text "Run"]
        ,button [onClick LoadEx] [text "LoadEx"]
        ,hr [] []
        ,div [] [text <| .dotString model]
        ,hr [] []
        ,svgOrNot model
        ,hr [] []
        ,div [] [text <| cleanSvgString <| .image model]
        ,hr [] []
        ,div [] [text <| .image model]
        ]

type Msg 
    = Run
    | Clear
    | LoadEx 
    | SaveProg String
    | SaveFilt String
    | GotDot (Result Http.Error String)
    | GotSvg (Result Http.Error String)

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
            (   {model | program = "", filter = "", image = ""}
            , Cmd.none
            )
        LoadEx ->
            (   {model | program = exampleProg, filter = exampleFilter, image = ""}
            , Cmd.none
            )
        SaveProg prog ->
            ( {model | program = prog}
            , Cmd.none 
            )
        SaveFilt filt ->
            ( {model | filter = filt}
            , Cmd.none
            )
        Run ->
            ( model, getDotString model )
        GotDot (Ok dot) ->
            ( {model | dotString = dot}, getSvg {model | dotString = dot} )
        GotDot (Err httpError) ->
            ( { model | dotString = buildErrorMessage httpError}
            , Cmd.none
            )
        GotSvg (Ok svg) ->
            ( {model | image = svg}, Cmd.none)
        GotSvg (Err httpError) ->
            ( { model | image = buildErrorMessage httpError}
            , Cmd.none
            )



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
        , body = Http.jsonBody <| JE.object [ ("diagram_source", JE.string (.dotString model)) ]
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

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
