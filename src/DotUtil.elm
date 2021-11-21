module DotUtil exposing (getRefList, getLabels, shortenLabels)

import DotLang
import Html.Parser exposing (nodeToString)
import Ellipsis 

-- Function to return a list of referenced nodes (These need to have their IDs shown in the GUI)
-- This relies on reference nodes being an ellipse, while nothing else is
getRefList : String -> List String
getRefList str = List.map showId <| findRefNodes str

--Crudely converts IDs to a String, should be fine for these purposes
showId : DotLang.ID -> String
showId dotID = case dotID of
   DotLang.ID str -> str
   DotLang.HtmlID node -> nodeToString node
   DotLang.NumeralID numid -> String.fromFloat numid

findRefNodes : String -> List DotLang.ID
findRefNodes str =
    case DotLang.fromString str of
    Ok (DotLang.Dot DotLang.Digraph _ x) -> listRefs [] x
    _ -> []

listRefs : List DotLang.ID  -> List DotLang.Stmt -> List DotLang.ID
listRefs rs stmts = case stmts of
    [] -> rs
    (x::xs) -> case x of
        DotLang.NodeStmt (DotLang.NodeId _ _) y -> if isRef y
            then case getLabelID y of
                Nothing -> listRefs rs xs
                Just m -> listRefs (m::rs) xs
            else listRefs rs xs
        _ -> listRefs rs xs

isRef : List DotLang.Attr -> Bool
isRef attrs = case attrs of
    [] -> False
    (y::ys) -> case y of
        DotLang.Attr (DotLang.ID "shape") (DotLang.ID "ellipse") -> True
        _ -> isRef ys

getLabelID : List DotLang.Attr -> Maybe DotLang.ID
getLabelID attrs = case attrs of
    [] -> Nothing
    (y::ys) -> case y of
        DotLang.Attr (DotLang.ID "label") match -> Just match
        _ -> getLabelID ys

-- Function to get
getLabels : String -> List (String, String)
getLabels str = 
    case DotLang.fromString str of
    Ok (DotLang.Dot DotLang.Digraph _ x) -> idTupleToStrings <| getLabelsHelper [] x
    _ -> []

getLabelsHelper : List (DotLang.ID, DotLang.ID) -> List DotLang.Stmt -> List (DotLang.ID, DotLang.ID)
getLabelsHelper rs stmts = case stmts of
    [] -> rs
    (x::xs) -> case x of
        DotLang.NodeStmt (DotLang.NodeId id _) y -> case getLabelID y of
            Nothing -> getLabelsHelper rs xs
            Just m -> getLabelsHelper ((id, m)::rs) xs
        _ -> getLabelsHelper rs xs

idTupleToStrings : List (DotLang.ID, DotLang.ID) -> List (String, String)
idTupleToStrings input = case input of
    [] -> []
    ((idA, idB)::rest) -> ((showId idA, showId idB)::(idTupleToStrings rest))

--Function to shorten the shown label text for Dot graph nodes
shortenLabels : String -> List (String, String) -> Int ->  List String -> String
shortenLabels input nodeMap maxLength tokens = 
    case DotLang.fromString input of
        Ok (DotLang.Dot DotLang.Digraph id x) -> DotLang.toString <| DotLang.Dot DotLang.Digraph id (shortenLabelsHelper x nodeMap maxLength tokens)
        _ -> input 

shortenLabelsHelper : List DotLang.Stmt -> List (String, String) -> Int -> List String -> List DotLang.Stmt
shortenLabelsHelper stmts nodeMap maxLength tokens = case stmts of
    [] -> []
    (x::xs) -> case x of
        DotLang.NodeStmt (DotLang.NodeId id p) y -> 
            let newLabelStr = Ellipsis.shorten (nodeMapMatch id nodeMap) maxLength tokens
                newAttrList = replaceAttr y "label" newLabelStr
                newStmt = DotLang.NodeStmt (DotLang.NodeId id p) newAttrList
            in
                 newStmt :: shortenLabelsHelper xs nodeMap maxLength tokens
        _ -> x :: shortenLabelsHelper xs nodeMap maxLength tokens

nodeMapMatch : DotLang.ID -> List (String, String) -> String
nodeMapMatch id nodeMap = case nodeMap of
    [] -> ""
    ((node, label)::rest) -> if showId id == node 
        then label
        else nodeMapMatch id rest

replaceAttr : List DotLang.Attr -> String -> String -> List DotLang.Attr
replaceAttr attrs attrMatch newValue = case attrs of
    [] -> []
    ((DotLang.Attr atType atVal)::rest) -> 
        if showId atType == attrMatch
        then (DotLang.Attr atType (DotLang.ID newValue)) :: rest
        else (DotLang.Attr atType atVal) :: (replaceAttr rest attrMatch newValue)