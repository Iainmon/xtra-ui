module SvgUtil exposing (..)

-- Remove text prior to <svg> tag (Required step for SVG parsing)
cleanFront : String -> String
cleanFront str = (++) "<svg" <| List.foldr (++) "" <| List.drop 1 <| String.split "<svg" str

