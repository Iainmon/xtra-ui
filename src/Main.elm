port module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Events exposing (..)

import Html.Attributes exposing (rows, cols, style, type_, checked, placeholder, value, hidden, href)
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
--import Html.Attributes exposing (alt)

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
import Svg.Events exposing (on)
import Tuple3

import Bootstrap.Navbar as Navbar
import Bootstrap.Button as Button
import Task as Task
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Popover as Popover
import Bootstrap.Accordion as Accordion
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Dropdown as Dropdown
import List exposing (length)


type alias Size num =
    { width : num
    , height : num
    }

type alias ContextMenu =
    { nodeClicked : Int,
      mouseX : Int,
      mouseY : Int,
      show : Bool
    }
type alias Model = 
    { height : Int
    , width : Int 
    , program : String
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
    , errorType : Bool
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
    , nodeContext : ContextMenu
    , cbRes : Bool
    , navbarState : Navbar.State
    , cbPopover : Popover.State
    , accState : Accordion.State
    , ddState : Dropdown.State
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

type alias Flags = { url : String, height : Int, width : Int}

run : msg -> Cmd msg
run m = Task.perform (always m) (Task.succeed ())

{-
seqCmds : List msg -> Cmd msg
seqCmds msgs =
  let
    cmds = List.map run msgs
  in
    Task.perform (always )
-}
-- Initial state
-- Todo: Smarter initial values for size and center?
init : Flags -> (Model, Cmd Msg)
init flags =
    let 
      (navbarState, navbarCmd) = Navbar.initialState NavbarMsg
      model = initModel flags.url flags.height flags.width navbarState
      initCmd = Cmd.batch [navbarCmd, run GetInit]
    in
    case model.share of
        "" -> (model, initCmd)
        a -> case B64.decode a of
            Err b -> let modelErrShare = {model | share = "Error: " ++ b} in
                (modelErrShare, initCmd)
            Ok c -> 
                let modelGoodShare = {model | share = c}
                    sharedGraph = D.decodeString jsonToSavedGraph modelGoodShare.share
                in
                case sharedGraph of
                    Ok g -> 
                        let modelWithShareLoaded = loadSavedGraphToModel modelGoodShare g
                        in
                        (modelWithShareLoaded, initCmd)
                    _ -> (modelGoodShare, initCmd)

navBarHeight : Int
navBarHeight = 56

initModel : String -> Int -> Int -> Navbar.State -> Model
initModel location h w nb =
    { height = h - navBarHeight
    , width = w
    , program = ""
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
    , errorType = False
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
    , nodeContext = ContextMenu 0 0 0 False
    , cbRes = False
    , navbarState = nb
    , cbPopover = Popover.initialState
    , accState = Accordion.initialState
    , ddState = Dropdown.initialState
    }

port clipboardCopy : String -> Cmd msg
port clipboardRes : (Bool -> msg) -> Sub msg

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
                Nothing -> update (AlertMsg Alert.shown) {model | error = "Empty result", errorType = False}
                Just v -> (loadSavedGraphToModel model (decodeSavedGraphs v), Cmd.batch [run Run, run CloseFilterAcc ])
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
defaultInitData = {actions=[], funcFormat=(UT.NoFormat "="), filters=[], shortenTokens=[], examples=Dict.empty}

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

view : Model -> Browser.Document Msg
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
        makeSvg modelIn = if modelIn.svgString == "" then div [(String.fromInt modelIn.width) ++ "px" |> style "width", (String.fromInt modelIn.height) ++ "px" |> style "height", style "display" "flex", style "align-items" "center", style "justify-content" "center"] [text "Input a program to begin"] else
            case SU.svgNodeToSvgMod ((SU.addRefdNodes model.refNodes) >> SU.modTitle model.allNodeLabels) (SU.cleanFront (modelIn.svgString)) of
               Err msg -> div [(String.fromInt modelIn.width) ++ "px" |> style "width", (String.fromInt modelIn.height) ++ "px" |> style "height", style "display" "flex", style "align-items" "center", style "justify-content" "center"] [text msg]
               Ok (_, svgs) -> -- _ is attrs
                    Svg.svg
                        [ num Attr.width <| toFloat modelIn.width
                        , num Attr.height <| toFloat modelIn.height
                        , handleZoom Zoom
                        , Draggable.mouseTrigger () DragMsg
                        , clickHandler NodeClicked
                        , style "cursor" "pointer"
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
      { title = "Tracr.app"
      , body = 
        [ Navbar.config NavbarMsg
          |> Navbar.withAnimation
          |> Navbar.collapseSmall 
          |> Navbar.brand [href "#", style "cursor" "pointer"] [text "Tracr.app"]
          |> Navbar.info
          |> Navbar.items
            [ Navbar.dropdown
              { id = "loadDD"
              , toggle = Navbar.dropdownToggle [] [text "Load"]
              , items = 
                Navbar.dropdownHeader [text "Saved Programs"]
                :: List.map (\s -> Navbar.dropdownItem [onClick <| LoadSavedGraph s] [text s]) model.savedGraphKeys
                ++ Navbar.dropdownHeader [text "Example Programs"]
                :: (Dict.keys model.initData.examples |> List.map (\s -> Navbar.dropdownItem [onClick <| LoadEx s] [text s]))
              }
            , Navbar.dropdown
              { id = "DeleteDD"
              , toggle = Navbar.dropdownToggle [] [text "Delete"]
              , items = 
                Navbar.dropdownHeader [text "Saved Programs"]
                :: List.map (\s -> Navbar.dropdownItem [onClick <| DeleteSavedGraph s] [text s]) model.savedGraphKeys
              }
            , Navbar.itemLink
              [onClick <| CBCopy shareUrl, style "cursor" "pointer"]
              [text "Share"]
          --  , Navbar.itemLink
          --    [onClick <| ShowModal About, style "cursor" "pointer"]
          --    [text "About"]

            ]
          |> Navbar.customItems
            [ Navbar.formItem []
              [ Input.text [Input.attrs [placeholder "Enter a Name to Save", onInput UpdateName, value model.saveName]]
              , Button.button
                [ Button.success
                , Button.onClick SaveGraph
                , Button.attrs [Spacing.ml2Sm]
                ]
                [text "Save Program"]
              ]
            ]
          |> Navbar.view model.navbarState
        , div [ style "vertical-align" "top", style "position" "absolute", style "right" "0%", style "bottom" "0%", style "width" "550px", style "z-index" "3" ] 
          [ Accordion.config AccMsg
            |> Accordion.withAnimation
            |> Accordion.onlyOneOpen
            |> Accordion.cards
              [ Accordion.card
                { id = "progCard"
                , options = []
                , header = Accordion.header [] <| Accordion.toggle [] [text "Program"]
                , blocks =
                  [ Accordion.block []
                    [ Block.custom (textarea [onInput SaveProg, value model.program, rows 4, cols 40] [])]
                  ]
                }
              , Accordion.card
                { id = "filtCard"
                , options = []
                , header = Accordion.header [] <| Accordion.toggle [] [text "Filters"]
                , blocks =
                  [ Accordion.block []
                      [Block.text [] [Button.button [Button.success, Button.block, Button.large, Button.attrs [onClick AddFilter, style "cursor" "pointer"]] [text "Add New Filter (+)"]]]
                  , Accordion.listGroup ({-filterCardBlockGhost model model.items ::-} (List.indexedMap (filterCardBlock model) model.items))
                  ]
--                  , Accordion.block []
--                    [Block.custom (div [] [model.items |> List.indexedMap (itemView model) |> Html.div [], ghostView model model.items])]
--                  ]
                }
              ]
            |> Accordion.view model.accState
          ]
        , div [ style "vertical-align" "bottom", style "position" "absolute", style "right" "0%", style "top" "56px", style "width" "400px", style "z-index" "3" ] 
          [ div [style "width" "400px"]
            [ Alert.config
              |> (if model.errorType then Alert.info else Alert.danger)
              |> Alert.dismissableWithAnimation AlertMsg
              |> Alert.children
                  [ Alert.h4 [] [ text ( if model.errorType then "Success" else "Error") ]
                  , text <| model.error
                  ]
              |> Alert.view model.errorVis
            ]
          ]
        , div [style "border-style" "none"] [ makeSvg model ]
        , showContext model
        ]
      }

clickHandler : ((String, Int, Int) -> msg) -> Svg.Attribute msg
clickHandler msg = 
  let targetId = D.at ["target", "parentNode", "id"] D.string
      mouseX = D.at ["clientX"] D.int
      mouseY = D.at ["clientY"] D.int
  in
  Svg.Events.on "click" (D.map msg <| D.map3 Tuple3.join targetId mouseX mouseY)

showContext : Model -> Html Msg
showContext mdl =
  let ddState = (.ddState mdl)
  in
  if mdl.nodeContext.show then 
    div 
      [ style "position" "absolute"
      , style "left" (String.fromInt mdl.nodeContext.mouseX |> flip (++) "px")
      , style "top" (String.fromInt mdl.nodeContext.mouseY |> flip (++) "px")
      , style "border" "1px solid black"
      --, style "background-color" "rgb(200,200,200)"
      --, style "display" "table-row"
      ]
      [ListGroup.custom
        [ ListGroup.button [ListGroup.active] [text "Global"]
        , ListGroup.button [ListGroup.attrs [onClick <| NodeClicked ("",0,0)]] [text "Placeholder 1"]
        , ListGroup.button [ListGroup.attrs [onClick <| NodeClicked ("",0,0)]] [text "Placeholder 2"]
        , ListGroup.button [ListGroup.active] [text "Local"]
        , ListGroup.button [ListGroup.attrs [onClick <| NodeClicked ("",0,0)]] [text "Placeholder 3"]
        , ListGroup.button [ListGroup.attrs [onClick <| NodeClicked ("",0,0)]] [text "Placeholder 4"]
        ]
      ]
{-      [ div [] ["Node Selected: " ++ (String.fromInt mdl.nodeContext.nodeClicked) |> text]
      , div [] ["MouseX: " ++ (String.fromInt mdl.nodeContext.mouseX) |> text]
      , div [] ["MouseY: " ++ (String.fromInt mdl.nodeContext.mouseY) |> text]
      ]-}
  else
    div [] []

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

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

--exampleItems : List String -> List (DropdownItem msg)
exampleItems ex = List.map (\s -> Navbar.dropdownItem [onClick <| LoadEx s] [text s]) ex

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


filterCardBlock : Model -> Int -> Filter -> ListGroup.Item Msg
filterCardBlock model index item =
  let
    itemId = "id-filt" ++ String.fromInt item.id
    dnd = model.dnd
    dropEvent = system.dropEvents index itemId
    dragEvent = system.dragEvents index itemId
    liColor = if item.enabled then ListGroup.success else ListGroup.danger
  in
  case system.info dnd of
    Just {dragIndex} ->
      if dragIndex /= index then
        ListGroup.li [liColor]
          [ Html.div
              (Html.Attributes.id itemId :: dropEvent)--:: system.dropEvents index itemId)
              [filterView model item "" []]--[Html.text <| filtToString model item ]
          ]
      else
        ListGroup.li [liColor]
          [ Html.div
              [Html.Attributes.id itemId]
              [Html.div [] [filterView model item "" []]]--text "<--- Move Filter Here --->" ]]
          ]
    Nothing ->
      ListGroup.li [liColor]
        [ Html.div
            [Html.Attributes.id itemId] --(Html.Attributes.id itemId :: [])--(Html.Attributes.id itemId :: dragEvent)--:: system.dragEvents index itemId)
            [filterView model item "" dragEvent]--[Html.text <| filtToString model item ]
        ]
      
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
                Html.div
                    (Html.Attributes.id itemId :: dropEvent)--:: system.dropEvents index itemId)
                    [filterView model item "" []]--[Html.text <| filtToString model item ]
            else
                Html.div
                    [Html.Attributes.id itemId]
                    [Html.div [style "background-color" "grey"] [ text "<--- Move Filter Here --->" ]]
        Nothing ->
            Html.div
                [Html.Attributes.id itemId] --(Html.Attributes.id itemId :: [])--(Html.Attributes.id itemId :: dragEvent)--:: system.dragEvents index itemId)
                [filterView model item "" dragEvent]--[Html.text <| filtToString model item ]

filterCardBlockGhost : Model -> List Filter -> ListGroup.Item Msg
filterCardBlockGhost model items =
  let
    dnd = model.dnd
    maybeDragItem : Maybe Filter
    maybeDragItem = system.info dnd
      |> Maybe.andThen (\{dragIndex} -> items |> List.drop dragIndex |> List.head)
  in
  case maybeDragItem of
    Just item ->
      ListGroup.li [if item.enabled then ListGroup.success else ListGroup.danger]
          [ Html.div
              (style "opacity" "0.5" :: system.ghostStyles dnd)--:: system.dropEvents index itemId)
              [filterView model item "" []]--[Html.text <| filtToString model item ]
          ]
    Nothing ->
      ListGroup.li [] []


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
    in
    Grid.container []
      ( Grid.row []
        [ Grid.col [Col.mdAuto, Col.attrs []] 
          [ Button.button 
              [ Button.outlineLight
              , Button.small
              , Button.attrs ([Html.Attributes.id idStr, Spacing.m1, style "cursor" "grab"] ++ event)
              ] 
              [ text "\u{2630}" ]
          , Button.button 
              [ if item.enabled then Button.outlineSuccess else Button.outlineDanger
              , Button.onClick (ToggleCheck item.id)
              , Button.small
              , Button.attrs [Spacing.m1]
              ]
              [ if item.enabled then text "On" else text "Off" ]
          , Button.button 
              [ if item.actionIndex == 1 then Button.success else Button.danger
              , Button.onClick (SetAction item.id (if item.actionIndex == 1 then 0 else 1))
              , Button.small
              , Button.attrs [Spacing.m1]
              ]
              [div (if item.actionIndex == 1 then [] else [style "text-decoration" "line-through"]) [text "Propagate"]]
          ]
        , Grid.col [Col.md5 ]
          [ select [Html.Events.on "change" (D.map (SetFilter item.id) targetValueIntParse), style "height" "35px", style "width" "210px"] --drop down list with filters from model, at currently selected index 
            (List.map (filterOption model.initData.filters item.selectIndex) <| List.range 0 <| (+) (-1) <| List.length model.initData.filters)
          ]
        , Grid.col [Col.mdAuto]
          [ Button.button 
              [ Button.outlineDanger
              , Button.small
              , Button.onClick (RemoveFilter item.id)
              , Button.attrs [Spacing.ml1]--[Spacing.ml5]
              ]
              [text "Delete"]
          ]
        ] 
      ::
      ( if not showParam then [] else
        [ Grid.row []
            [ Grid.col [Col.md4] []
            , Grid.col [Col.mdAuto]
              [ input [hidden <| not showParam, style "margin-left" "35px",placeholder "Filter parameter", value item.param, onInput (UpdateFilterParam item.id)] []--input text box for parameters IF the current filter has one (with event handler for changing)
              ]
            ]
        ]
      ))

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
loadSavedGraphToModel model gr = 
  let maxFiltId = List.foldl (\x acc -> max x.id acc) 0 gr.filters
  in
  {model | program= gr.program, items=gr.filters, filtIdCounter=maxFiltId+1}  

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

type ModalType
  = Save
  | Load
  | Share
  | About

--Msg data type, for events
type Msg 
    = Run
    | Clear
    | LoadEx String
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
    | AlertMsg Alert.Visibility
    | SaveGraph
    | Process JE.Value
    | ListGraphKeys
    | UpdateName String
    | LoadSavedGraph String
    | DeleteSavedGraph String
    | RunShare
    | NodeClicked (String, Int, Int)
    | CBCopy String
    | CBRes Bool
    | ResizeView Int Int
    | NavbarMsg Navbar.State
    | PopoverMsg Popover.State
    | AccMsg Accordion.State
    | CloseFilterAcc
    | OpenFilterAcc
    | DDMsg Dropdown.State

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
        LoadEx name ->
          case Dict.get name model.initData.examples of
            Just n -> ({model | error = "Example Loaded", errorType = True}, Cmd.batch[run <| SaveProg n, run <| AlertMsg Alert.shown] )
            Nothing -> ({model | error = "Example Not Loaded", errorType = False}, Cmd.batch[run <| AlertMsg Alert.shown] )
        SaveProg prog ->
            ({model | program = prog}, run Run)
        SaveFilt filt ->
            ({model | filter = filt}, run Run)
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
                    maxLabelLength = 50 -- replace with model variable
                    matchTokens = ["=","⇒","of","in", ";"] -- replace with model variable (recieved from backend)
                    shortenedDot = DU.shortenLabels dot newNodeLabels maxLabelLength matchTokens
                    newModel = {model | dotString = dot, shortDotString = shortenedDot, refNodes = newRefNodes, allNodeLabels = newNodeLabels}
                in ( newModel, getSvg newModel )
            err -> update (AlertMsg Alert.shown) {model | error = err, errorType = False}


        GotDot (Err httpError) ->
            ( { model | dotString = buildErrorMessage httpError}
            , Cmd.none
            )
        GotSvg (Ok svg) ->
            update (AlertMsg Alert.closed) {model | svgString = SU.stringFindAndReplace svg SU.svgReplace} 
            -- SLOW (but complete): ( {model | svgString = unescape svg}, Cmd.none)
        GotSvg (Err httpError) ->
            update (AlertMsg Alert.shown) { model | error = buildErrorMessage httpError, errorType = False}
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
                cmds = Cmd.batch [run Run, run <| CloseFilterAcc]
            in
            ({ model | items = newFilters, filtIdCounter=model.filtIdCounter + 1},  cmds)-- Run { model | items = newFilters, filtIdCounter=model.filtIdCounter + 1}
        RemoveFilter target ->
            let
                targetIndex = List.Extra.findIndex (\filt -> filt.id == target) model.items
                newFilters = case targetIndex of
                   Just index -> List.Extra.removeAt index model.items
                   Nothing -> model.items
                cmds = Cmd.batch [run Run, run <| CloseFilterAcc]
            in
            ({ model | items = newFilters }, cmds)
        AlertMsg vis ->
            if model.dirty then
                update ListGraphKeys { model | errorVis = vis, dirty = False }
            else
                ({ model | errorVis = vis }, Cmd.none)
        LoadSavedGraph name ->
          let
            upModel = {model | saveName = name, error = "Program Loaded", errorType = True}
            cmds = Cmd.batch [(send (LocalStorage.get name) upModel), run (AlertMsg Alert.shown)]
          in
          (upModel, cmds)
        SaveGraph ->
            if List.member model.saveName model.savedGraphKeys then
                update (AlertMsg Alert.shown) { model | error = "Please enter a unique name to save your program", errorType = False}
            else
                case model.saveName of
                    "" -> update (AlertMsg Alert.shown) { model | error = "Please enter a name to save your program", errorType = False}
                    name ->
                        let newModel = {model | dirty=True, savedGraphKeys=(name :: model.savedGraphKeys), selectedSaveGraph=0, saveName="", error = "Saved as " ++ name, errorType = True}
                            cmds = Cmd.batch [send (LocalStorage.put name (Just <| savedGraphToJSON <| getCurrentGraph model)) newModel, run (AlertMsg Alert.shown)]
                        in
                        (newModel, cmds)
        Process value ->
            case PortFunnels.processValue funnelDict value model.funnelState model
            of
                Err error -> update (AlertMsg Alert.shown) { model | error = error, errorType = False}
                Ok res -> res
        ListGraphKeys ->
            {model | savedGraphKeys = []} |> withCmd (send (LocalStorage.listKeys "") model)
        UpdateName value ->
            {model | saveName = value} |> withNoCmd
        DeleteSavedGraph name ->
          let 
            updatedKeys = List.Extra.remove name model.savedGraphKeys
            upModel = {model | error = "Program Saved", savedGraphKeys=updatedKeys, errorType = True}
            cmds = Cmd.batch [send (LocalStorage.put name Nothing) upModel, run <| AlertMsg Alert.shown]
          in
          (upModel, cmds)

        RunShare ->
            case model.share of
                "" -> (model,Cmd.none)
                _ -> (model,getDotString model)
        NodeClicked (nodeStr,x,y) ->
            if nodeStr |> String.startsWith "node" then
              case String.dropLeft 4 nodeStr |> String.toInt of
                  Nothing -> ({model | nodeContext = ContextMenu 0 0 0 False}, Cmd.none)
                  Just node -> ({model | nodeContext = ContextMenu node x y True}, Cmd.none)
            else
              ({model | nodeContext = ContextMenu 0 0 0 False}, Cmd.none)
        CBCopy str -> (model, clipboardCopy str)
        CBRes _ -> ({model | error = "Link copied to Clipboard", errorType = True}, run <| AlertMsg Alert.shown)
        ResizeView w h -> ( { model | width = w, height = h - navBarHeight }, Cmd.none )
        NavbarMsg state -> ({model | navbarState = state}, Cmd.none)
        PopoverMsg state -> ({model | cbPopover = state}, Cmd.none)
        AccMsg state -> ( { model | accState = state } , Cmd.none )
        CloseFilterAcc ->
          ({model | accState = Accordion.initialState}, run OpenFilterAcc)
        OpenFilterAcc -> 
          ({model | accState = Accordion.initialStateCardOpen "filtCard"}, Cmd.none)
        DDMsg state -> ( { model | ddState = state } , Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Draggable.subscriptions DragMsg model.drag
        , system.subscriptions model.dnd
        , Alert.subscriptions model.errorVis AlertMsg
        , PortFunnels.subscriptions Process model
        , clipboardRes CBRes
        , Browser.Events.onResize (\width height -> ResizeView width height)
        , Navbar.subscriptions model.navbarState NavbarMsg
        , Accordion.subscriptions model.accState AccMsg
        , Dropdown.subscriptions model.ddState DDMsg
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

main : Program Flags Model Msg
main =
    Browser.document
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



