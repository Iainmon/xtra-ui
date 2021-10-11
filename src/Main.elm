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

type alias Model = { program : String, filter : String, image : String}

init : () -> (Model, Cmd Msg)
init _ = 
    ( { program = "", filter = "", image = ""}
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
        [textarea [rows 8, cols 60] [text <| .program model]
        ,hr [] []
        ,textarea [rows 8, cols 60] [text <| .filter model]
        ,hr [] []
        ,button [onClick Clear] [text "Clear"]
        ,button [onClick Run] [text "Run"]
        ,button [onClick LoadEx] [text "LoadEx"]
        ,hr [] []
        ,div [] [text <| .image model]
        ]

type Msg 
    = Run
    | Clear
    | LoadEx
    | GotDot (Result Http.Error String)


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
        Run ->
            ( model, getDotString model )
        GotDot (Ok dot) ->
            ( {model | image = dot}, Cmd.none)
        GotDot (Err httpError) ->
            ( { model | image = buildErrorMessage httpError}
            , Cmd.none
            )



getDotString : Model -> Cmd Msg
getDotString model = 
{--
    Http.post
    {   url = xtraBackendURL
        , expect = Http.expectJson GotDot (D.field "dot" D.string)
        , body = Http.jsonBody <| JE.object [ ("prog", JE.string (.program model)), ("filter", JE.string (.filter model))]
    }
--}
{--}
    Http.riskyRequest
    {   url = xtraBackendURL
        , method = "GET"
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson GotDot (D.field "dot" D.string)
        , body = Http.jsonBody <| JE.object [ ("prog", JE.string (.program model)), ("filter", JE.string (.filter model))]
    }
--}


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
