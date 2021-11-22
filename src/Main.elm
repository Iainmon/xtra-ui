module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Draggable exposing (Msg)
import Html.Attributes exposing (rows, cols)
import Http
import Json.Decode as D
import Json.Encode as JE
import Draggable exposing (init)
import SvgParser exposing( SvgNode(..))
import Svg exposing (svg)
import Html.Attributes exposing (style)
import Svg.Attributes as Attr
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY)
import List.Extra

import DotUtil as DU
import SvgUtil as SU
import SvgUtil exposing (modTitle)

type alias Size num =
    { width : num
    , height : num
    }
type alias Model = 
    { program : String
    , filter : String
    , dotString : String
    , svgString : String
    , refNodes : List String
    , allNodeLabels : List (String, String)
    , shortDotString : String
    , zoom : Float
    , center : Vec2
    , size : Size Float
    , drag : Draggable.State ()
    }

-- Initial state
-- Todo: Smarter initial values for size and center?
init : flags -> (Model, Cmd Msg)
init _ = 
    ( { program = ""
      , filter = ""
      , dotString = ""
      , svgString = ""
      , refNodes = []
      , allNodeLabels = []
      , shortDotString = ""
      , zoom = 1
      , center = Vector2.vec2 375 500--750 500
      , size = Size 1500 1000
      , drag = Draggable.init
      }
    , Cmd.none
    )

--From Draggable pan/zoom example
dragConfig : Draggable.Config () Msg
dragConfig =
    Draggable.basicConfig (OnDragBy << (\( dx, dy ) -> Vector2.vec2 dx dy))

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
    let
        ( cx, cy ) =
            ( getX model.center, getY model.center )

        ( halfWidth, halfHeight ) =
            ( model.size.width / model.zoom / 2, model.size.height / model.zoom / 2 )

        ( top, left ) =
            ( cy - halfHeight, cx - halfWidth )

        ( _, _ ) = --(bottom, right)
            ( cy + halfHeight, cx + halfWidth )

        panning =
            "translate(" ++ String.fromFloat -left ++ ", " ++ String.fromFloat -top ++ ")"

        zooming =
            "scale(" ++ String.fromFloat model.zoom ++ ")"
        makeSvg modelIn = if modelIn.svgString == "" then div [style "width" "1500px", style "height" "1000px", style "display" "flex", style "align-items" "center", style "justify-content" "center"] [text "Input a program to begin"] else
            case SU.svgNodeToSvgMod ((SU.addRefdNodes model.refNodes) >> modTitle model.allNodeLabels) (SU.cleanFront (modelIn.svgString)) of
               Err msg -> div [style "width" "1500px", style "height" "1000px", style "display" "flex", style "align-items" "center", style "justify-content" "center"] [text msg]
               Ok (_, svgs) -> -- _ is attrs
                    Svg.svg
                        [ num Attr.width modelIn.size.width
                        , num Attr.height modelIn.size.height
                        , handleZoom Zoom
                        , Draggable.mouseTrigger () DragMsg
                      --  , onClick SVGClicked
                        ]
                        [(Svg.g 
                            [ Attr.transform (zooming ++ " " ++ panning)
                            , Attr.id "graph0"
                            , Attr.class "graph"
                            ] svgs 
                        )]
    in
    table [] 
        [tr [] 
            [th [] []--[text "Input"]
            ,th [] []--[text "Output"]
            ]
        , tr []
            [td [ style "vertical-align" "top" ] 
                [hr [] []
                ,textarea [rows 15, cols 60, onInput SaveProg] [text <| .program model]
                ,hr [] []
                ,textarea [rows 25, cols 60, onInput SaveFilt] [text <| .filter model]
                ,hr [] []
                ,div [style "display" "grid", style "padding" "20px", style "padding-inline" "30px", style "grid-gap" "10px"]
                    [button [onClick Clear] [b [] <| [text "Clear"]]
                    ,button [onClick Run] [strong [] <| [text "Run"]]
                    ,button [onClick LoadEx] [b [] <| [text "Load Example"]]
                    ]
        {--     ,hr [] []
                ,textarea [rows 8, cols 60, onInput ManualSvg] [text <| (.svgString model) ]
                ,hr [] []
                ,textarea [rows 8, cols 60, onInput ManualDot] [text <| (.dotString model) ]
                ,hr [] []
                ,textarea [rows 8, cols 60] [text <| String.join ", " <|(.refNodes model) ]
                ,hr [] []
                ,textarea [rows 8, cols 60] [text <| showStrTupList <|(.allNodeLabels model) ] 
                ,hr [] []
                ,textarea [rows 8, cols 60] [text <|(.shortDotString model) ] --} 
                ]
            ,td [style "border-style" "double"] [ makeSvg model ]
            ]
        ]

--From Draggable pan/zoom example
num : (String -> Svg.Attribute msg) -> Float -> Svg.Attribute msg
num attr value =
    attr (String.fromFloat value)

--From Draggable pan/zoom example
handleZoom : (Float -> msg) -> Svg.Attribute msg
handleZoom onZoom =
    let
        alwaysPreventDefaultAndStopPropagation msg =
            { message = msg, stopPropagation = True, preventDefault = True }

        zoomDecoder : D.Decoder msg
        zoomDecoder =
            D.float
                |> D.field "deltaY"
                |> D.map (onZoom)
    in
    Html.Events.custom
        "wheel"
    <|
        D.map alwaysPreventDefaultAndStopPropagation zoomDecoder

--Msg data type, for events
type Msg 
    = Run
    | Clear
    | LoadEx 
    | SaveProg String
    | SaveFilt String
    | GotDot (Result Http.Error String)
    | GotSvg (Result Http.Error String)
    | ManualSvg String
    | ManualDot String
    | DragMsg (Draggable.Msg ())
    | OnDragBy Vec2
    | Zoom Float

-- Now outdated function to  get Svg to embed
{-- 

svgOrNot : Model -> Html Msg
svgOrNot model = 
    if .svgString model == "" then
        div [] []
    else
        case parseWithNodeMod (modNodeTextLength << modTitle) (SU.cleanFront (.svgString model)) of
            Err msg -> div [] [text msg]
            Ok elem -> elem

--}


-- Master update function for processing event msgs
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Clear ->
            init ()
        LoadEx ->
            update Run {model | program = exampleProg, filter = exampleFilter, svgString = ""}
        SaveProg prog ->
            update Run {model | program = prog}
        SaveFilt filt ->
            update Run {model | filter = filt}
        Run ->
            ( model, getDotString model )
        GotDot (Ok dot) ->
            let newRefNodes = List.reverse <| List.Extra.unique <| DU.getRefList dot
                newNodeLabels = List.reverse <| DU.getLabels dot
                maxLabelLength = 35 -- replace with model variable
                matchTokens = ["=","⇒","of","in", ";"] -- replace with model variable (recieved from backend)
                shortenedDot = DU.shortenLabels dot newNodeLabels maxLabelLength matchTokens
                newModel = {model | dotString = dot, shortDotString = shortenedDot, refNodes = newRefNodes, allNodeLabels = newNodeLabels}
            in ( newModel, getSvg newModel )
        GotDot (Err httpError) ->
            ( { model | dotString = buildErrorMessage httpError}
            , Cmd.none
            )
        GotSvg (Ok svg) ->
            ( {model | svgString = SU.stringFindAndReplace svg SU.svgReplace}, Cmd.none)-- SLOW (but complete): ( {model | svgString = unescape svg}, Cmd.none)
        GotSvg (Err httpError) ->
            ( { model | svgString = buildErrorMessage httpError}
            , Cmd.none
            )
        ManualDot dot ->
            ( {model | dotString = dot}, getSvg {model | dotString = dot} )
        ManualSvg svg ->
            ( {model | svgString = SU.stringFindAndReplace svg SU.svgReplace}, Cmd.none )
        OnDragBy rawDelta ->
            let
                delta =
                    rawDelta
                        |> Vector2.scale (-1 / model.zoom)
            in
            ( { model | center = model.center |> Vector2.add delta }, Cmd.none )
        Zoom factor ->
            let
                newZoom =
                    model.zoom
                        |> (+) (factor * 0.002)
                        |> clamp 0.5 5
            in
            ( { model | zoom = newZoom }, Cmd.none )
        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag

--Htto request code for getting Dot String from server
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

--Http request code for getting SvgString from Kroki Container
getSvg : Model -> Cmd Msg
getSvg model =
    Http.post
    { url = krokiURL
        , expect = Http.expectString GotSvg
        , body = Http.jsonBody <| JE.object [ ("diagram_source", JE.string (.shortDotString model)), ("layout", JE.string "dot") ]
    }

--Http error to String message funciton
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

-- This code for replacing Node numbers with Letters is no longer needed
{-
nodePattern : String
nodePattern = "\">[0-9].</FONT>"

--nodeTest = "<FONT COLOR=\"#0000ff\" POINT-SIZE=\"9\">66</FONT>\n<FONT COLOR=\"#0000ff\" POINT-SIZE=\"9\">65</FONT>"
beforeNode : String
beforeNode = "POINT-SIZE=\"9\">"
afterNode : String
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
       ((n :: ns),(a :: alphs)) -> (wrapNodeMatch (String.fromInt n), wrapNodeMatch a) :: genReplaceNode ns alphs

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
                Maybe.withDefault 0 (String.toInt (String.slice 2 -7 y)) :: pullIDs xs

--"[0-9]. -> [0-9].;"
--"<FONT COLOR="#0000ff" POINT-SIZE="9">[0-9].<\/FONT>"

nodeRegex : Regex.Regex
nodeRegex = Maybe.withDefault Regex.never (Regex.fromString nodePattern)


-- This kinda works, boundaries are wonky, but ok for now. fix later.
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
-}


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Convienience function to show the node map list (id, label) (used for debugging, probably not required anymore)
showStrTupList : List (String, String) -> String
showStrTupList input = case input of
   [] -> ""
   (x::xs) -> (showStrTup x " = ") ++ "\n" ++ showStrTupList xs
showStrTup : (String, String) -> String -> String
showStrTup (first, second) sep = first ++ sep ++ second


