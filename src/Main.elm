module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)

import Html.Attributes exposing (rows, cols, style, type_, checked, placeholder, value, hidden)
import Html.Events.Extra exposing (targetValueIntParse)
import Http
import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as JE
import Draggable exposing (init, Msg)
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY)
import DnDList
import Bootstrap.Alert as Alert

import SvgParser exposing( SvgNode(..))
import Svg exposing (svg)
import Svg.Attributes as Attr

import List.Extra

import DotUtil as DU
import SvgUtil as SU
import CustomDecoders as CD
import UITypes as UT
import List.Extra exposing (updateIf)
import Html.Attributes exposing (alt)

import PortFunnel.LocalStorage as LocalStorage
    exposing ( Key, Message, Response(..))

import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd, andThen)
import PortFunnels exposing (FunnelDict, Handler(..))
import Dict exposing (Dict)
import Dict exposing (empty)
import Url.Parser.Query as PQ
import Url.Parser exposing ((<?>))
import Url.Parser as UP
import Url
import Base64 as B64


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
    , exampleIndex : Int
    , error : String
    , errorVis : Alert.Visibility
    , useSimulator : Bool
    , wasLoaded : Bool
    , funnelState : PortFunnels.State
    , savedGraphKeys : List String
    , saveName : String
    , selectedSaveGraph : Int
    , url : Maybe Url.Url
    , share : String
    , generatedShare : String
    , dirty : Bool
    }

queryParser : String -> String
queryParser url = case Url.fromString url of
    Nothing -> ""
    Just a -> case a.query of
        Nothing -> ""
        Just b -> b

prefix : String
prefix =
    "xtraui"

type alias Flags = { url : String }

-- Initial state
-- Todo: Smarter initial values for size and center?
init : Flags -> (Model, Cmd Msg)
init flags =
    let model = initModel flags.url in
    case model.share of
        "" -> update GetInit model
        a -> case B64.decode a of
            Err b -> let modelErrShare = {model | share = "Error: " ++ b} in
                update GetInit <| modelErrShare
            Ok c -> 
                let modelGoodShare = {model | share = c}
                    sharedGraph = D.decodeString jsonToSavedGraph modelGoodShare.share
                in
                case sharedGraph of
                    Ok g -> 
                        let modelWithShareLoaded = loadSavedGraphToModel modelGoodShare g
                        in
                        update GetInit <| modelWithShareLoaded
                    _ -> update GetInit <| modelGoodShare


initModel : String -> Model
initModel location = 
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
    , exampleIndex = 0
    , error = ""
    , errorVis = Alert.closed
    , useSimulator = False
    , wasLoaded = False
    , funnelState = PortFunnels.initialState prefix
    , savedGraphKeys = []
    , saveName = ""
    , selectedSaveGraph = 0
    , url = Url.fromString location
    , share = queryParser location -- location --
    , generatedShare = ""
    , dirty = False
    }

doIsLoaded : Model -> Model
doIsLoaded model =
    if not model.wasLoaded && LocalStorage.isLoaded model.funnelState.storage then
        { model | useSimulator = False, wasLoaded = True}
    else model

storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state mdl =
    let model = doIsLoaded { mdl | funnelState = state }
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            case value of
                Nothing -> update (AlertMsg Alert.shown) {model | error = "Empty result"}
                Just v -> update Run (loadSavedGraphToModel model (decodeSavedGraphs v))
        LocalStorage.ListKeysResponse { label, keys } ->
            update RunShare {model | savedGraphKeys = keys}
            --{model | savedGraphKeys = keys} |> withCmd Run
        _ ->  update ListGraphKeys model

funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict [ LocalStorageHandler storageHandler ] getCmdPort

getCmdPort : String -> Model -> (JE.Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort Process moduleName model.useSimulator

send : Message -> Model -> Cmd Msg
send message model =
    LocalStorage.send (getCmdPort LocalStorage.moduleName model)
        message
        model.funnelState.storage

defaultInitData : UT.InitType
defaultInitData = {actions=[], funcFormat=(UT.NoFormat "="), filters=[], shortenTokens=[], examples=[]}

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
        shareB64 = getCurrentGraph model |> savedGraphToJSON |> JE.encode 0 |> B64.encode
        baseUrl = case model.url of
            Nothing -> ""
            Just a -> Url.toString a |> String.split "?" |> List.head |> Maybe.withDefault ""
        shareUrl = baseUrl ++ "?" ++ shareB64
    in
    table [] 
        [tr [] 
            [th [] []
            ,th [] []
            ]
        , tr []
            [td [ style "vertical-align" "top" ] 
                [hr [] []
                ,textarea [rows 10, cols 60, onInput SaveProg, value model.program] []
                ,hr [] []
                ,button [onClick AddFilter] [b [] <| [text "(+) Add New Filter"]]
                ,div [style "text-align" "center"] 
                [
                    model.items |> List.indexedMap (itemView model) |> Html.div [], ghostView model model.items]
                ,hr [] []
                ,div [style "margin" "0 auto", style "display" "flex", style "justify-content" "center"]
                    [select [on "change" (D.map SetExample targetValueIntParse), style "width" "200px", style "height" "50px", style "margin" "10px", style "font-size" "x-large"]
                        (List.map (exampleOption model.initData.examples model.exampleIndex) <| List.range 0 <| (+) (-1) <| List.length model.initData.examples)
                    ,button [onClick LoadEx, style "margin" "10px", style "font-size" "medium"] [b [] <| [text "Load Example"]]
                    ,button [onClick Clear, style "margin" "10px", style "font-size" "medium"] [b [] <| [text "Clear All"]]
                    ]
                
                ,div [style "margin" "0 auto", style "display" "flex", style "justify-content" "center"] [
                    input [onInput UpdateName, value model.saveName, style "width" "200px"] []
                    ,button [onClick SaveGraph, style "margin" "10px", style "font-size" "medium"] [b [] <| [text "Save Program"]]
                ]
                ,hr [] []
                ,div [style "margin" "0 auto", style "display" "flex", style "justify-content" "center"]
                    [select [on "change" (D.map SetTargetSaved targetValueIntParse), style "width" "200px", style "height" "50px", style "margin" "10px", style "font-size" "x-large"]
                        (List.map (savedGraphOption model.savedGraphKeys model.selectedSaveGraph) <| List.range 0 <| (+) (-1) <| List.length model.savedGraphKeys)
                    ,button [onClick LoadSavedGraph, style "margin" "10px", style "font-size" "medium"] [b [] <| [text "Load"]]
                    ,button [onClick DeleteSavedGraph, style "margin" "10px", style "font-size" "medium"] [b [] <| [text "Delete"]]
                    ]
                ,div [style "width" "550px"] [
                    Alert.config
                        |> Alert.info
                        |> Alert.dismissableWithAnimation AlertMsg
                        |> Alert.children
                            [ Alert.h4 [] [ text "Error" ]
                            , text <| model.error
                            ]
                        |> Alert.view model.errorVis
                    ]
                ,hr [] []
                ,textarea [rows 10, cols 60, value shareUrl] []
                ]
            ,td [style "border-style" "double"] [ makeSvg model ]
            ]
        ]

savedGraphOption : List String -> Int -> Int -> Html msg
savedGraphOption keys selInd index =
    let
        name = List.Extra.getAt index keys
        sel = Html.Attributes.selected (index == selInd)
    in
    case name of
        Just savedName -> option [value <| String.fromInt index, sel] [text savedName]
        _ -> option [value "-1"] [text "Invalid example index"]

exampleOption : List (String, String) -> Int -> Int -> Html msg
exampleOption examples selInd index =
    let
        examp = List.Extra.getAt index examples
        sel = Html.Attributes.selected (index == selInd)
    in
    case examp of
        Just (exampName, _) -> option [value <| String.fromInt index, sel] [text exampName]
        _ -> option [value "-1"] [text "Invalid example index"]


filtsToString : Model -> String
filtsToString model = 
    let
        notEnabledInds = List.Extra.findIndices (\filt->not filt.enabled) model.items
        enabledItems = List.Extra.removeIfIndex (\index->List.member index notEnabledInds) model.items
    in
    String.join "\n" <| List.map (\filt -> String.trim <| filtToString model filt) enabledItems

filtToString : Model -> Filter -> String
filtToString model item = 
    let
        actionText = case List.Extra.getAt item.actionIndex model.initData.actions of
           Just str -> str ++ " "
           Nothing -> ""

        filt = Maybe.withDefault ("",True) <| List.Extra.getAt item.selectIndex model.initData.filters
        filtText = Tuple.first filt
        hasParam = Tuple.second filt
    in
    if item.enabled then 
        if hasParam then
        actionText ++ filtText ++ " " ++ item.param
        else
        actionText ++ filtText
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

actionOption : List String -> Int -> Int -> Html msg
actionOption actions selInd index =
    let
        action = List.Extra.getAt index actions
        sel = Html.Attributes.selected (index == selInd)
    in
    case action of
        Just actionText -> option [value <| String.fromInt index, sel] [text actionText]
        _ -> option [value "-1"] [text "Invalid filter index"]

filterView : Model -> Filter -> String -> List (Html.Attribute Msg) -> Html.Html Msg
filterView model item idStr event =
    let
        showParam : Bool
        showParam = Tuple.second <| Maybe.withDefault ("",False) <| List.Extra.getAt item.selectIndex model.initData.filters
        color = if item.enabled then "green" else "red"
    in
    Html.div [style "background-color" color]
        [ Html.input [type_ "checkbox", onClick <| ToggleCheck item.id, checked item.enabled] []
        , select [on "change" (D.map (SetAction item.id) targetValueIntParse)] --drop down list with actions
            (List.map (actionOption model.initData.actions item.actionIndex) <| List.range 0 <| (+) (-1) <| List.length model.initData.actions)
        , select [on "change" (D.map (SetFilter item.id) targetValueIntParse)] --drop down list with filters from model, at currently selected index 
            (List.map (filterOption model.initData.filters item.selectIndex) <| List.range 0 <| (+) (-1) <| List.length model.initData.filters)
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
    { actionIndex : Int
    , enabled : Bool
    , selectIndex : Int
    , param : String
    , id : Int
    }

filterToJSON : Filter -> JE.Value
filterToJSON input = 
    JE.object 
        [ ("actionIndex", JE.int input.actionIndex)
        , ("enabled", JE.bool input.enabled)
        , ("selectIndex", JE.int input.selectIndex)
        , ("param", JE.string input.param)
        , ("id", JE.int input.id)
        ]

jsonToFilterList : D.Decoder (List Filter)
jsonToFilterList = D.field "filters" (D.list jsonToFilter)

jsonToFilter : D.Decoder (Filter)
jsonToFilter = 
    D.map5 Filter 
        (D.field "actionIndex" D.int) 
        (D.field "enabled" D.bool) 
        (D.field "selectIndex" D.int) 
        (D.field "param" D.string) 
        (D.field "id" D.int)

type alias SavedGraph =
    { program : String
    , filters : List Filter
    }

savedGraphsToJSON : List SavedGraph -> JE.Value
savedGraphsToJSON input =
    JE.list savedGraphToJSON input

savedGraphToJSON : SavedGraph -> JE.Value
savedGraphToJSON input =
    JE.object
        [ ("program", JE.string input.program)
        , ("filters", JE.list filterToJSON input.filters)
        ]

decodeSavedGraphs : JE.Value -> SavedGraph
decodeSavedGraphs value =
    case D.decodeValue jsonToSavedGraph value of
        Ok res -> res
        Err err -> {program = D.errorToString err, filters=[]}


jsonToSavedGraph : D.Decoder (SavedGraph)
jsonToSavedGraph = 
    D.map2 SavedGraph
        (D.field "program" D.string)
        (jsonToFilterList)

getCurrentGraph : Model -> SavedGraph
getCurrentGraph model = {program = model.program, filters = model.items}

loadSavedGraphToModel : Model -> SavedGraph -> Model
loadSavedGraphToModel model gr = {model | program= gr.program, items=gr.filters}  

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
    | GotDot (Result Http.Error (String,String))
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
    | SetAction Int Int
    | AddFilter
    | RemoveFilter Int
    | SetExample Int
    | AlertMsg Alert.Visibility
    | SaveGraph
    | Process JE.Value
    | ListGraphKeys
    | UpdateName String
    | SetTargetSaved Int
    | LoadSavedGraph
    | DeleteSavedGraph
    | RunShare

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
            ({model | items = [], program = "", svgString = ""}, Cmd.none)
        LoadEx ->
            let example = List.Extra.getAt model.exampleIndex model.initData.examples
            in
            case example of
               Just (_, exampleText) -> update (SaveProg exampleText) model
               _ -> update (SaveProg "") model
        SaveProg prog ->
            update Run {model | program = prog}
        SaveFilt filt ->
            update Run {model | filter = filt}
        Run ->
            ( model, getDotString model )
        GetInit ->
            ( model, getInitData model)
        GotInit (Ok data) ->
            update ListGraphKeys { model | initData = data, filters = data.filters} 
        GotInit (Err httpError) -> -- Problem
            ( model, Cmd.none)
        GotDot (Ok (dot, error)) ->
            case error of
            "" -> 
                let newRefNodes = List.reverse <| List.Extra.unique <| DU.getRefList dot
                    newNodeLabels = List.reverse <| DU.getLabels dot
                    maxLabelLength = 35 -- replace with model variable
                    matchTokens = ["=","⇒","of","in", ";"] -- replace with model variable (recieved from backend)
                    shortenedDot = DU.shortenLabels dot newNodeLabels maxLabelLength matchTokens
                    newModel = {model | dotString = dot, shortDotString = shortenedDot, refNodes = newRefNodes, allNodeLabels = newNodeLabels}
                in ( newModel, getSvg newModel )
            err -> update (AlertMsg Alert.shown) {model | error = err}


        GotDot (Err httpError) ->
            ( { model | dotString = buildErrorMessage httpError}
            , Cmd.none
            )
        GotSvg (Ok svg) ->
            update (AlertMsg Alert.closed) {model | svgString = SU.stringFindAndReplace svg SU.svgReplace} 
            -- SLOW (but complete): ( {model | svgString = unescape svg}, Cmd.none)
        GotSvg (Err httpError) ->
            update (AlertMsg Alert.shown) { model | error = buildErrorMessage httpError}
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
        SetAction target index ->
            let
                newFilters = List.Extra.updateIf (\filt -> filt.id == target) (\filt -> {filt | actionIndex = index}) model.items
            in
            update Run { model | items = newFilters }

        AddFilter ->
            let
                newFilters = model.items ++ [{actionIndex=0, enabled=False, selectIndex=0, param="", id=model.filtIdCounter}]
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
        SetExample index ->
            ({model | exampleIndex = index}, Cmd.none)
        AlertMsg vis ->
            if model.dirty then
                update ListGraphKeys { model | errorVis = vis, dirty = False }
            else
                ({ model | errorVis = vis }, Cmd.none)
        LoadSavedGraph ->
            let targetKey = List.Extra.getAt model.selectedSaveGraph model.savedGraphKeys
            in
            case targetKey of
                Just targ -> {model | saveName = targ} |> withCmd (send (LocalStorage.get targ) {model | saveName = targ})
                _ -> update (AlertMsg Alert.shown) { model | error = "Selected Saved Program has an invalid index" }
        SaveGraph ->
            if List.member model.saveName model.savedGraphKeys then
                update (AlertMsg Alert.shown) { model | error = "Please enter a unique name to save your program" }
            else
                case model.saveName of
                    "" -> update (AlertMsg Alert.shown) { model | error = "Please enter a name to save your program" }
                    name ->
                        let newModel = {model | dirty=True, savedGraphKeys=(name :: model.savedGraphKeys), selectedSaveGraph=0, saveName=""}
                        in
                        newModel |> withCmd (send (LocalStorage.put name (Just <| savedGraphToJSON <| getCurrentGraph model)) newModel)
        Process value ->
            case PortFunnels.processValue funnelDict value model.funnelState model
            of
                Err error -> update (AlertMsg Alert.shown) { model | error = error }
                Ok res -> res
        ListGraphKeys ->
            {model | savedGraphKeys = []} |> withCmd (send (LocalStorage.listKeys "") model)
        UpdateName value ->
            {model | saveName = value} |> withNoCmd
        SetTargetSaved index ->
            ({model | selectedSaveGraph = index}, Cmd.none)
        DeleteSavedGraph ->

            case List.Extra.getAt model.selectedSaveGraph model.savedGraphKeys of
                Just targ -> 
                    let updatedKeys = List.Extra.remove targ model.savedGraphKeys
                        upModel = {model | selectedSaveGraph = max 0 (model.selectedSaveGraph - 1), savedGraphKeys=updatedKeys} 
                    in
                    upModel |> withCmd (send (LocalStorage.put targ Nothing) upModel)
                _ -> update (AlertMsg Alert.shown) { model | error = "Selected Saved Program has an invalid index" }
        RunShare ->
            case model.share of
                "" -> (model,Cmd.none)
                _ -> (model,getDotString model)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Draggable.subscriptions DragMsg model.drag
        , system.subscriptions model.dnd
        , Alert.subscriptions model.errorVis AlertMsg
        , PortFunnels.subscriptions Process model
        ]
    

--Http request code for getting Dot String from server
getDotString : Model -> Cmd Msg
getDotString model = 
    Http.post
    { url = xtraBackendURL
        , expect = Http.expectJson GotDot (D.succeed Tuple.pair 
                                            |> DE.andMap (D.field "dot" D.string)
                                            |> DE.andMap (D.field "error" D.string))
        , body = Http.jsonBody <| JE.object [ ("prog", JE.string (.program model)), ("filter", JE.string (filtsToString model))]
    }

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

main : Program {url : String} Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Convienience function to show the node map list (id, label) (used for debugging, probably not required anymore)
{-
showStrTupList : List (String, String) -> String
showStrTupList input = case input of
   [] -> ""
   (x::xs) -> (showStrTup x " = ") ++ "\n" ++ showStrTupList xs
showStrTup : (String, String) -> String -> String
showStrTup (first, second) sep = first ++ sep ++ second
-}



