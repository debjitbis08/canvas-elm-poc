module Atoms.AtomList exposing (atoms)

import Html exposing (..)

import Atoms.Atom exposing (Atom)
import Atoms.FilledCircle exposing (filledCircle)

atoms : List (Atom msg)
atoms =
  [ filledCircle ]
