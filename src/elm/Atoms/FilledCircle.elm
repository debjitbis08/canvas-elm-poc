module Atoms.FilledCircle exposing ( filledCircle )

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict
import String

import Atoms.Atom exposing (..)

listHtml : Html msg
listHtml =
  span [ class "round filled", style styles.list ][]

canvasHtml : List Property -> Html msg
canvasHtml properties =
  let props = Dict.fromList (List.map (\p -> (p.name, p.value)) properties)
  in
  span [ style (styles.canvas props) ][]

properties : List Property
properties =
  [ { name = "Color", value = TextProp "#FF0000" }
  , { name = "Size", value = TextProp "16" }
  ]

filledCircle : Atom msg
filledCircle = { listHtml = listHtml, canvasHtml = canvasHtml, properties = properties }

mapStyles : PropertyType -> String
mapStyles prop =
  case prop of
    TextProp v -> v
    IntProp v -> toString v
    FloatProp v -> toString v

-- CSS STYLES
styles : { list : List ( String, String ), canvas : Dict.Dict String PropertyType -> List ( String, String ) }
styles =
  {
    list =
      [ ( "display", "inline-block" )
      , ( "width", "16px" )
      , ( "height", "16px")
      , ( "background", "#337AB7")
      , ( "border-radius", "8px" )
      ]
    , canvas = \props ->
      let size = Result.withDefault 16 (String.toInt (mapStyles (Maybe.withDefault (TextProp "16") (Dict.get "Size" props))))
      in
      List.concat
        [ [ ( "background", mapStyles (Maybe.withDefault (TextProp "red") (Dict.get "Color" props)))
          , ( "border-radius", (toString (size//2)) ++ "px" )
          , ( "width", (toString size) ++ "px" )
          , ( "height", (toString size) ++ "px" )
          ]
        , [ ( "display", "inline-block" ) ]
        ]
  }
