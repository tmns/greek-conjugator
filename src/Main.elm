import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Json
import Task

main : Program (Maybe Model) Model Msg
main =
  Browser.document
  { init = init
  , view = \model -> { title = "Greek Conjugator", body = [view model] }
  , update = update
  , subscriptions = \_ -> Sub.none
  }



-- MODEL


type alias Model =
  { conjugation : Dict String String
  , field : String
  }


-- type alias FullConjugation =
--   { present : SingleConjugation 
--   , simplePast : SingleConjugation
--   , pastContinuous : SingleConjugation
--   , simpleFuture : SingleConjugation
--   , futureContinuous : SingleConjugation
--   , imperative : SingleConjugation
--   , particple : SingleConjugation
--   }

-- type alias SingleConjugation =
--   { aEnikos : String
--   , bEnikos : String
--   , gEnikos : String
--   , aPlithintikos : String
--   , bPlithintikos : String
--   , gPlithintikos : String
--   }


emptyModel : Model 
emptyModel =
  { conjugation = Dict.empty 
  , field = ""
  }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
  ( Maybe.withDefault emptyModel maybeModel 
  , Cmd.none
  )



-- UPDATE

type Msg
  = NoOp
  | UpdateField String
  | ConjugateField



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )


    UpdateField str ->
      ( { model | field = str }
      , Cmd.none
      )

    
    ConjugateField ->
      ( { model 
          | field = ""
          , conjugation =
            if String.endsWith "άω" model.field then
              Dict.fromList
                [ ("aEnikos", model.field)
                , ("bEnikos", String.dropRight 1 model.field ++ "ς")
                , ("gEnikos", String.dropRight 1 model.field ++ "ει")
                , ("aPlithintikos", String.dropRight 1 model.field ++ "με")
                , ("bPlithintikos", String.dropRight 1 model.field ++ "τε")
                , ("gPlithintikos", String.dropRight 1 model.field ++ "νε")                  
                ]
            
            else if String.endsWith "ώ" model.field then
              Dict.fromList
                [ ("aEnikos", model.field)
                , ("bEnikos", String.dropRight 1 model.field ++ "είς")
                , ("gEnikos", String.dropRight 1 model.field ++ "εί")
                , ("aPlithintikos", String.dropRight 1 model.field ++ "ούμε")
                , ("bPlithintikos", String.dropRight 1 model.field ++ "είτε")
                , ("gPlithintikos", String.dropRight 1 model.field ++ "ούνε")
                ]

            else
              Dict.fromList
                [ ("aEnikos", model.field)
                , ("bEnikos", String.dropRight 1 model.field ++ "εις")
                , ("gEnikos", String.dropRight 1 model.field ++ "ει")
                , ("aPlithintikos", String.dropRight 1 model.field ++ "ουμε")
                , ("bPlithintikos", String.dropRight 1 model.field ++ "ετε")
                , ("gPlithintikos", String.dropRight 1 model.field ++ "ουνε")
                ]
        }
        , Cmd.none
      )
        

-- VIEW

view : Model -> Html Msg
view model = 
  div [] 
    [ section []
      [ lazy viewInput model.field 
      , lazy viewConjugations model.conjugation
      ]
    ]

viewInput : String -> Html Msg
viewInput verb =
  header []
    [ h1 [] [ text "Greek Conjugator" ]
    , input
      [ placeholder "What verb would you like to conjugate?"
      , autofocus True
      , value verb
      , name "verb"
      , onInput UpdateField
      , onEnter ConjugateField
      ]
      []
    ]

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    isEnter code =
      if code == 13 then
        Json.succeed msg
      else
        Json.fail "not ENTER"
  in
    on "keydown" (Json.andThen isEnter keyCode)

viewConjugations : Dict String String -> Html Msg
viewConjugations conjugation =
  section []
  [ div [] <|
    List.map viewConj (Dict.values conjugation)
  ]

viewConj : String -> Html Msg
viewConj conjVal =
  p [] [ text conjVal ]
  

  