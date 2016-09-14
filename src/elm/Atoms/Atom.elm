module Atoms.Atom exposing (..)

import Html exposing (..)

type PropertyType
  = TextProp String
  | IntProp Int
  | FloatProp Float

type alias Property = { name: String, value: PropertyType }

type alias Atom msg =
  { listHtml: Html msg
  , canvasHtml: List (Property) -> Html msg
  , properties: List Property
  }
