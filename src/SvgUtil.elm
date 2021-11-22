module SvgUtil exposing (..)

import SvgParser exposing(parse, parseToNode, nodeToSvg, SvgNode(..), toAttribute, Element, SvgAttribute)
import Svg exposing (Attribute, Svg, node, svg)
import Html exposing (..)
import Round
import Browser exposing (element)
--import Svg.Attributes exposing (restart)

-- Function: Remove text prior to <svg> tag (Required step for SVG parsing)
cleanFront : String -> String
cleanFront str = (++) "<svg" <| List.foldr (++) "" <| List.drop 1 <| String.split "<svg" str

-- Intended to crudely replace html character codes
-- This way (pick and choose relevant) is much faster than an all-inclusive solution (unescape function)
stringFindAndReplace : String -> List (String,String) -> String
stringFindAndReplace source list =
    case list of
       [] -> source
       ((match, replacement) :: xs) -> stringFindAndReplace (String.replace match replacement source) xs

-- Basic map of html character codes and their replacement
svgReplace : List (String,String)
svgReplace =    [("&quot;","\"")
                ,("&#45;","-")
                ,("&#32;"," ")
                ,("&#160;"," ")
                ,("&gt;",">")]

--------------------------------
-- Begin SVG Functions

--Checks a list of svgNodes to see if it contains a title node
svgNodeListHasTitle : List SvgNode -> Bool
svgNodeListHasTitle nodes =
    case nodes of
        [] -> False
        (n::ns) -> 
            case n of
            SvgElement elem -> if elem.name == "title" then True else svgNodeListHasTitle ns
            _   -> svgNodeListHasTitle ns

-- Modifies an SvgNodes innerText, but Only if it's a title
changeTitleNodeText : String -> SvgNode -> SvgNode
changeTitleNodeText str node =
    case node of
        SvgElement elem ->   if elem.name == "title"
                            then
                                SvgElement {elem | children = [SvgText str] }
                            else
                                node
        _ -> node

-- Gets a List of all SvgText Strings for all child SvgNodes
getAllNodesText : SvgNode -> List String
getAllNodesText node = 
    case node of
       SvgComment _ -> []
       SvgText str -> [str]
       SvgElement elem -> if elem.name == "title" then [] else List.foldr (\x->\t-> getAllNodesText x ++ t) [] elem.children

--Changes Titles to node contents following these rules:
--    If SVGNode has a child SvgElement with name "title"
--        Then modify that child's child SvgText to equal SVGNode's child SvgText
modTitle : List (String,String) -> SvgNode -> SvgNode
modTitle replaceMap node = case node of
    SvgComment _ -> node
    SvgText _ -> node
    SvgElement elem ->  if svgNodeListHasTitle elem.children 
                        then --newTitle = getAllNodesText node in
                        let nodeId = getNodeTitle elem.children 
                            newTitle = newTitleFromMap replaceMap nodeId
                            newChildren = List.map (changeTitleNodeText newTitle) elem.children 
                        in 
                            SvgElement {elem | children = List.map (modTitle replaceMap) newChildren}
                        else SvgElement {elem | children = List.map (modTitle replaceMap) elem.children}

newTitleFromMap : List (String,String) -> String -> String
newTitleFromMap replaceMap nodeId = case List.filter (\(b,c)->nodeId==b) replaceMap of
    [] -> nodeId
    (match::empty) -> Tuple.second match

modNodeTextLength : SvgNode -> SvgNode
modNodeTextLength node = case node of
    SvgComment _ -> node
    SvgText str -> if (String.length str) > maxNodeLength then SvgText ((String.left (maxNodeLength - 3) str) ++ "...") else node
    SvgElement elem -> if elem.name == "title" then node else SvgElement {elem | children = List.map modNodeTextLength elem.children}

maxNodeLength : Int
maxNodeLength = 20

addRefdNodes : List String -> SvgNode -> SvgNode
addRefdNodes listRef node = case node of
    SvgComment _ -> node
    SvgText _ -> node
    SvgElement elem -> if svgNodeListHasTitle elem.children
        then let nodeTitle = getNodeTitle elem.children in
            if List.member nodeTitle listRef 
            then SvgElement {elem | children=(addRefLabel node nodeTitle)::elem.children}
            else SvgElement {elem | children = List.map (addRefdNodes listRef) elem.children}
        else SvgElement {elem | children = List.map (addRefdNodes listRef) elem.children}

--Add a referenced node label, given that SvgNode is a districtly defined type, 
--   this only really works in a specific case. Functions called prior assure this will only 
--   operate on the "correct" SvgNode (a node with a child that's text). idk if I need to copy all required (positional) attributes here...
addRefLabel : SvgNode -> String -> SvgNode
addRefLabel node label = case node of
    SvgElement elem -> 
        let textLength = findTextLength elem.children 
            childTextAttrs = getTextAttrsFromList elem.children
        in
            refLabel childTextAttrs textLength label
    _ -> node

getTextAttrsFromList : List SvgNode -> List SvgParser.SvgAttribute
getTextAttrsFromList nodes = case nodes of
    [] -> []
    (node::rest) -> case node of
        SvgElement elem -> 
            if elem.name == "text"
            then elem.attributes
            else getTextAttrsFromList rest
        _ -> getTextAttrsFromList rest


--This function expects the list of SvgNodes to be the children of a node in the DAG
findTextLength : List SvgNode -> Float
findTextLength nodes = case nodes of
    [] -> 0
    (node::rest) -> case node of
        SvgElement elem -> 
            if elem.name == "text"
            then getChildsLength elem.children
            else findTextLength rest
        _ -> findTextLength rest

--This function expects a list of child node that is SvgText, isn't helpful otherwise
getChildsLength : List SvgNode -> Float
getChildsLength nodes = case nodes of
   [] -> 0
   (node::rest) -> case node of
      SvgText str -> toFloat <| String.length str
      _ -> getChildsLength rest

--Predefined format for a Referenced Label, may or may not look correct (yet)
refLabel : List SvgAttribute -> Float -> String -> SvgNode
refLabel attrs siblingLength str = 
    let dxStr = (Round.round 2 <| siblingLength / 3) ++ "em" in
    (SvgElement 
        { name="text"
        , attributes=attrs++[("fill","blue"), ("font-family","Times,serif"),("dx",dxStr),("dy","-.5em"),("text-anchor","start")]
        , children=[SvgText str]
        }
    )

--Function to get the node title, This contains the refereneced node id via kroki conversion
getNodeTitle : List SvgNode -> String
getNodeTitle nodes = case nodes of
    [] -> ""
    (node::rest) -> case node of
        SvgElement elem -> 
            if elem.name == "title" 
            then nodeTextFromNodeList elem.children
            else getNodeTitle rest
        _ -> getNodeTitle rest


--This gets only bare text elements, not even children, not ideal but should work in this context
nodeTextFromNodeList : List SvgNode -> String
nodeTextFromNodeList nodes = case nodes of
    (SvgText str)::rest -> str ++ (nodeTextFromNodeList rest)
    (_::rest) -> nodeTextFromNodeList rest
    [] -> ""

--TODO: Function to take SvgNode, modify the titleText


--Modified version of SvgParser parse function to include (SvgNode -> SvgNode) modifier function before converting to html
--ToDo: This may again require modification for onCliick??
parseWithNodeMod : (SvgNode -> SvgNode) -> String -> Result String (Html msg)
parseWithNodeMod mod input =
    let 
        toHtml svgNode =
            case mod svgNode of
                SvgElement element ->
                    if element.name == "svg" then
                        Ok <|
                            svg (List.map toAttribute element.attributes)
                                (List.map nodeToSvg element.children)

                    else
                        Err "Top element is not svg"

                _ ->
                    Err "Top element is not svg"
    in
    parseToNode input |> Result.andThen toHtml

svgNodeToSvgMod : (SvgNode -> SvgNode) -> String -> Result String (List (Html.Attribute msg), List (Svg msg)) 
svgNodeToSvgMod mod input =
    let 
        toHtml svgNode = 
            case mod svgNode of
            SvgElement element ->
                    if element.name == "svg" then
                        Ok ((List.map toAttribute element.attributes), (List.map nodeToSvg element.children))
                    else Err "Top element is not svg"
            _ -> 
                    Err "Top element is not svg"
    in
    parseToNode input |> Result.andThen toHtml

