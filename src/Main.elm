module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)

import Html.Attributes exposing (rows, cols, style, type_, checked, placeholder, value, hidden)
import Html.Events.Extra exposing (targetValueIntParse)
import Http
import Json.Decode as D
import Json.Encode as JE
import Draggable exposing (init, Msg)
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY)
import DnDList

import SvgParser exposing( SvgNode(..))
import Svg exposing (svg)
import Svg.Attributes as Attr

import List.Extra

import DotUtil as DU
import SvgUtil as SU
import CustomDecoders as CD
import UITypes as UT



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
    , initData : UT.InitType
    , filters : List (String,Bool)
    , dnd : DnDList.Model
    , items : List Filter
    , filtIdCounter : Int
    }

-- Initial state
-- Todo: Smarter initial values for size and center?
init : flags -> (Model, Cmd Msg)
init _ = update GetInit initModel

initModel : Model
initModel = 
    { program = ""
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
    , initData = defaultInitData
    , filters = []
    , dnd = system.model
    , items = []
    , filtIdCounter = 0
    }

defaultInitData : UT.InitType
defaultInitData = {actions=[], funcFormat=(UT.NoFormat "="), filters=[], shortenTokens=[]}

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

xtraBackendURLInit : String
xtraBackendURLInit = "http://localhost:8081/init"

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
            case SU.svgNodeToSvgMod ((SU.addRefdNodes model.refNodes) >> SU.modTitle model.allNodeLabels) (SU.cleanFront (modelIn.svgString)) of
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
                ,textarea [rows 15, cols 60] [text <| filtsToString model]
                ,hr [] []
                ,textarea [rows 25, cols 60, onInput SaveFilt] [text <| .filter model]
                ,hr [] []
                ,div [style "text-align" "center"] 
                [
                    model.items |> List.indexedMap (itemView model) |> Html.div [], ghostView model model.items]
                ,hr [] []
                ,div [style "display" "grid", style "padding" "20px", style "padding-inline" "30px", style "grid-gap" "10px"]
                    [button [onClick AddFilter] [b [] <| [text "(+) Add New Filter"]]
                    ]
                ,hr [] []
                ,div [style "display" "grid", style "padding" "20px", style "padding-inline" "30px", style "grid-gap" "10px"]
                    [button [onClick Clear] [b [] <| [text "Clear"]]
                    ,button [onClick Run] [strong [] <| [text "Run"]]
                    ,button [onClick LoadEx] [b [] <| [text "Load Example"]]
                    ]
        {--       ,hr [] []
                ,textarea [rows 8, cols 60 ] [text <| UT.initTypeToString <| (.initData model) ]
        {--}        ,hr [] []
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

filtsToString : Model -> String
filtsToString model = 
    let
        notEnabledInds = List.Extra.findIndices (\filt->not filt.enabled) model.items
        enabledItems = List.Extra.removeIfIndex (\index->List.member index notEnabledInds) model.items
    in
    String.join "\n" <| List.map (filtToString model) enabledItems

filtToString : Model -> Filter -> String
filtToString model item = 
    let
        filt = Maybe.withDefault ("",True) <| List.Extra.getAt item.selectIndex model.filters
        filtText = Tuple.first filt
        hasParam = Tuple.second filt
    in
    if item.enabled then 
        if hasParam then
        filtText ++ " " ++ item.param
        else
        filtText
    else
        ""


itemView : Model -> Int -> Filter -> Html.Html Msg
itemView model index item =
    let
        itemId = "id-filt" ++ String.fromInt item.id
        dnd = model.dnd
        dropEvent = system.dropEvents index itemId
        dragEvent = system.dragEvents index itemId
    in
    case system.info dnd of
        Just {dragIndex} ->
            if dragIndex /= index then
                Html.p
                    (Html.Attributes.id itemId :: dropEvent)--:: system.dropEvents index itemId)
                    [filterView model item "" []]--[Html.text <| filtToString model item ]
            else
                Html.p
                    [Html.Attributes.id itemId]
                    [Html.div [style "background-color" "grey"] [ text "<--- Move Filter Here --->" ]]
        Nothing ->
            Html.p
                [Html.Attributes.id itemId] --(Html.Attributes.id itemId :: [])--(Html.Attributes.id itemId :: dragEvent)--:: system.dragEvents index itemId)
                [filterView model item "" dragEvent]--[Html.text <| filtToString model item ]

ghostView : Model -> List Filter -> Html.Html Msg
ghostView model items =
    let
        dnd = model.dnd
        maybeDragItem : Maybe Filter
        maybeDragItem = system.info dnd
            |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just item ->
            Html.p
                (style "opacity" "0.5" :: system.ghostStyles dnd)
                [filterView model item "" []]--[Html.text <| filtToString model item]
        Nothing ->
            Html.text ""

filterOption : List (String,Bool) -> Int -> Int -> Html msg
filterOption filts selInd index =
    let
        filt = List.Extra.getAt index filts
        sel = Html.Attributes.selected (index == selInd)
    in
    case filt of
        Just (filtText, _) -> option [value <| String.fromInt index, sel] [text filtText]
        _ -> option [value "-1"] [text "Invalid filter index"]

filterView : Model -> Filter -> String -> List (Html.Attribute Msg) -> Html.Html Msg
filterView model item idStr event =
    let
        showParam : Bool
        showParam = Tuple.second <| Maybe.withDefault ("",False) <| List.Extra.getAt item.selectIndex model.filters
        color = if item.enabled then "green" else "red"
    in
    Html.div [style "background-color" color]
        [ Html.input [type_ "checkbox", onClick <| ToggleCheck item.id, checked item.enabled] []
        , select [on "change" (D.map (SetFilter item.id) targetValueIntParse)] --drop down list with filters from model, at currently selected index 
            (List.map (filterOption model.filters item.selectIndex) <| List.range 0 <| (+) (-1) <| List.length model.filters)
        , input [hidden <| not showParam, placeholder "Filter parameter", value item.param, onInput (UpdateFilterParam item.id)] []--input text box for parameters IF the current filter has one (with event handler for changing)
        , button (Html.Attributes.id idStr :: style "cursor" "pointer" :: event) [text "\u{2630}"]  --, "handle", use a hamburger iron 
        , button [onClick <| RemoveFilter item.id] [text "\u{274C}"]
        ]

--Need way to add and remove filters (More msgs with update handler cases)
--  Also node click will be a shortcut to add a specific filter

--Finally, need to way to load the total filter list to a string (in order) to make a request 

----------------------------
-- Drag and Drop Code
----------------------------

-- Data

type alias Filter = 
    { enabled : Bool
    , selectIndex : Int
    , param : String
    , id : Int
    }

filt1 : Filter
filt1 = {enabled=True, selectIndex=0, param="", id=0}
filt2 : Filter
filt2 = {enabled=True, selectIndex=2, param="", id=1}
filt3 : Filter
filt3 = {enabled=True, selectIndex=1, param="", id=2}


testDnDdata : List Filter
testDnDdata = [filt1,filt2,filt3]

config : DnDList.Config Filter
config = 
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }

system : DnDList.System Filter Msg
system = DnDList.create config DndMsg

----------------------------
-- Draggable Pan/Zoom
----------------------------

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
    | GetInit
    | GotInit (Result Http.Error UT.InitType)
    | GotDot (Result Http.Error String)
    | GotSvg (Result Http.Error String)
    | ManualSvg String
    | ManualDot String
    | DragMsg (Draggable.Msg ())
    | OnDragBy Vec2
    | Zoom Float
    | DndMsg DnDList.Msg
    | ToggleCheck Int
    | SetFilter Int Int
    | UpdateFilterParam Int String
    | AddFilter
    | RemoveFilter Int

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
        GetInit ->
            ( model, getInitData model)
        GotInit (Ok data) ->
            ( { model | initData = data, filters = data.filters}, Cmd.none)
        GotInit (Err httpError) -> -- Problem
            ( model, Cmd.none)
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
        DndMsg message ->
            let 
                (dnd, items) =
                    system.update message model.dnd model.items
            in
            ( {model | dnd = dnd, items = items }
            , system.commands dnd 
            )
        ToggleCheck target ->
            let
                newFilters = List.Extra.updateIf (\filt -> filt.id == target) (\filt -> {filt | enabled = not filt.enabled}) model.items
            in
            update Run { model | items = newFilters }
        SetFilter target index ->
            let
                newFilters = List.Extra.updateIf (\filt -> filt.id == target) (\filt -> {filt | selectIndex = index}) model.items
            in
            update Run { model | items = newFilters }
        UpdateFilterParam target newParam ->
            let
                newFilters = List.Extra.updateIf (\filt -> filt.id == target) (\filt -> {filt | param = newParam}) model.items
            in
            update Run { model | items = newFilters }
        AddFilter ->
            let
                newFilters = model.items ++ [{enabled=False, selectIndex=0, param="", id=model.filtIdCounter}]
            in
            update Run { model | items = newFilters, filtIdCounter=model.filtIdCounter + 1}
        RemoveFilter target ->
            let
                targetIndex = List.Extra.findIndex (\filt -> filt.id == target) model.items
                newFilters = case targetIndex of
                   Just index -> List.Extra.removeAt index model.items
                   Nothing -> model.items
            in
            update Run { model | items = newFilters }



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Draggable.subscriptions DragMsg model.drag,
          system.subscriptions model.dnd
        ]
    

--Http request code for getting Dot String from server
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

getInitData : Model -> Cmd Msg
getInitData _ =
    Http.get
    { url = xtraBackendURLInit
    , expect = Http.expectJson GotInit CD.initDecoder }

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


