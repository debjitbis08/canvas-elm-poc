import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Json.Decode as Json
import Html.Events exposing (..)
import Mouse exposing (Position)
import String

-- component import example
import Atoms.Atom exposing (..)
import Atoms.AtomList exposing ( atoms )

-- APP
main : Program Never
main =
  App.program { init = init, view = view, update = update, subscriptions = subscriptions }

-- MODEL
type alias Model =
  { nextAtomId: Int
  , atomsInCanvas: List (String, Atom Msg, List Property)
  , draggedAtom: Maybe (Atom Msg)
  , selectedAtomId: Maybe String
  , atomDrag: Maybe Drag
  , atomDragPosition: Position
  , atomInDropLocation: Bool
  }

type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
  ( Model 0 [] Nothing Nothing Nothing (Position 15 10) False, Cmd.none )


-- UPDATE

type Msg
  = AtomDragStart (Atom Msg) Position
  | AtomDragAt Position
  | AtomDragEnd Position
  | AtomDrop
  | AtomSelect String
  | PropertyUpdated String PropertyType

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelper msg model, Cmd.none )

updateHelper : Msg -> Model -> Model
updateHelper msg model =
  case msg of
    AtomDragStart atom xy ->
      { model | atomDrag = (Just (Drag xy xy))
              , draggedAtom = Just atom }

    AtomDragAt xy ->
      { model | atomDrag = (Maybe.map (\{start} -> Drag start xy) model.atomDrag)
              , atomInDropLocation = isInDropLocation xy }

    AtomDragEnd _ ->
      { model | atomDragPosition = model.atomDragPosition
              , atomDrag = Nothing
              , atomInDropLocation = False
              , draggedAtom = Nothing
              , nextAtomId = model.nextAtomId + 1
              , atomsInCanvas =
                  if model.atomInDropLocation
                    then addAtomToCanvas model.atomsInCanvas model.draggedAtom model.nextAtomId
                    else model.atomsInCanvas }

    AtomDrop -> model
    AtomSelect atomId ->
      { model | selectedAtomId = Just atomId }
    PropertyUpdated name value ->
      case model.selectedAtomId of
        Nothing -> model
        Just selectedAtomId ->
          let updateProperty =
              \properties atom -> List.map (\prop -> if prop.name == name then { prop | value = value } else prop) properties
          in
          { model | atomsInCanvas = List.map
            (\(atomId, atom, props) ->
              if atomId == selectedAtomId then (atomId, atom, (updateProperty props atom)) else (atomId, atom, props))
            model.atomsInCanvas
          }

isInDropLocation : Position -> Bool
isInDropLocation position =
  position.x > 48 && position.y > 48
  && position.x < 548 && position.y < 248

addAtomToCanvas : List (String, Atom Msg, List Property) -> Maybe (Atom Msg) -> Int -> List (String, Atom Msg, List Property)
addAtomToCanvas atomsInCanvas a atomId =
  case a of
    Nothing -> atomsInCanvas
    Just atom -> List.append atomsInCanvas [ (toString atomId, atom, atom.properties) ]




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.atomDrag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves AtomDragAt, Mouse.ups AtomDragEnd ]

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib

view : Model -> Html Msg
view model =
  div [ class "main" ][
    h1 [][ text ( "Canvas POC" ) ]
    , div [ class "elements", style styles.elements ][
        ul [ class "list-group", style [("max-width", "200px")] ] (List.map (showElementInList (getPosition model)) atoms)
      ]
    , div [ class ("canvas" ++ ( if model.atomInDropLocation then " active" else ""))
          , style styles.canvas ]
      (List.map (\(atomId, atom, props) ->
        div [ class ("canvas-element" ++ (if Just (atomId) == model.selectedAtomId then " canvas-element-active" else ""))
            , id ( atomId )
            , onClick (AtomSelect atomId) ][ atom.canvasHtml props ]) model.atomsInCanvas)

    , div [ class "properties" ][ renderAtomProperties model.selectedAtomId model.atomsInCanvas ]
  ]

px : Int -> String
px number =
  toString number ++ "px"


getPosition : Model -> Position
getPosition {atomDragPosition, atomDrag} =
  case atomDrag of
    Nothing ->
      atomDragPosition

    Just {start,current} ->
      Position
        (atomDragPosition.x + current.x - start.x)
        (atomDragPosition.y + current.y - start.y)


onCustomMouseDown : Atom Msg -> Attribute Msg
onCustomMouseDown atom =
  on "mousedown" (Json.map (AtomDragStart atom) Mouse.position)

showElementInList : Position -> Atom Msg -> Html Msg
showElementInList dragPosition atom =
  li [ class "list-group-item elements-item" ][
    div [ class "actual" ][ atom.listHtml ]
    , div [ class "copy"
          , style (draggableStyles dragPosition)
          , onCustomMouseDown atom
          ][ atom.listHtml ]
  ]

textInput name v msg =
  input [ type' "text", value v, placeholder name, onInput msg ][]

renderProperty : Property -> Html Msg
renderProperty { name, value } =
  let propWithName = PropertyUpdated name
      propFromString = \s -> propWithName (TextProp s)
      propFromInt = \s -> propWithName (IntProp (Result.withDefault 0 (String.toInt s)))
      propFromFloat = \s -> propWithName (FloatProp (Result.withDefault 0 (String.toFloat s)))
  in
  case value of
    TextProp v -> textInput name v propFromString
    IntProp v -> textInput name (toString v) propFromInt
    FloatProp v -> textInput name (toString v) propFromFloat

renderAtomProperties : Maybe String -> List (String, Atom Msg, List Property) -> Html Msg
renderAtomProperties a atomsInCanvas =
  case a of
    Nothing -> div [][ text ("No atom selected") ]
    Just atomId ->
      let atomInCanvas = List.head (List.filter (\(id, _, _) -> id == atomId) atomsInCanvas)
      in
      case atomInCanvas of
        Nothing -> div [][ text ("No atom selected") ]
        Just (_, atom, properties) -> ul [] (List.map (\prop -> li [] [renderProperty prop]) properties)

-- CSS STYLES
styles =
  {
    img =
      [ ( "width", "33%" )
      , ( "border", "4px solid #337AB7")
      ],
    elements =
      [ ("float", "left")],
    canvas =
      [ ( "width", "500px" )
      , ( "height", "200px" )
      , ( "border", "1px solid red" )
      , ( "float", "left" )
      ]
  }

draggableStyles : Position -> List (String, String)
draggableStyles position =
  [ ("cursor", "move")
  , ("position", "absolute")
  , ("left", px position.x)
  , ("top", px position.y)
  ]
