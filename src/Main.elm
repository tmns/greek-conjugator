import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2)
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
  { conjugation : SingleConjugation
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

type alias SingleConjugation =
  { aEnikos : String
  , bEnikos : String
  , gEnikos : String
  , aPlithintikos : String
  , bPlithintikos : String
  , gPlithintikos : String
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
  | ConjugateField String



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )


    UpdateField str ->
      ( { model | field = str }
      , Cmd.none
      )

    
    ConjugateField str ->
          ( { model 
              | field = ""
              , conjugation =
                if String.endsWith "άω" str then
                  { aEnikos = str
                  , bEnikos = String.dropRight 1 str ++ "ς"
                  , gEnikos = String.dropRight 1 str ++ "ει"
                  , aPlithintikos = String.dropRight 1 str ++ "με"
                  , bPlithintikos = String.dropRight 1 str ++ "τε"
                  , gPlithintikos = String.dropRight 1 str ++ "νε"
                  } 
                
                else if String.endsWith "ώ" str then
                  { aEnikos = str
                  , bEnikos = String.dropRight 1 str ++ "είς"
                  , gEnikos = String.dropRight 1 str ++ "εί"
                  , aPlithintikos = String.dropRight 1 str ++ "ούμε"
                  , bPlithintikos = String.dropRight 1 str ++ "είτε"
                  , gPlithintikos = String.dropRight 1 str ++ "ούνε"
                  }                 
                else
                  { aEnikos = str
                  , bEnikos = String.dropRight 1 ++ "εις"
                  , gEnikos = String.dropRight 1 ++ "ει"
                  , aPlithintikos = String.dropRight 1 ++ "ουμε"
                  , bPlithintikos = String.dropRight 1 ++ "ετε"
                  , gPlithintikos = String.dropRight 1 ++ "ουνε"
                  } 
            }
            , Cmd.none
          )
        

-- VIEW

view : Model -> Html Msg
view model = 
  div [] 
    [ section
      [ lazy viewInput model.field 
      , lazy2 viewConjugation model.visibility model.conjugation
      ]
    ]

viewInput : String -> Html Msg
viewInput verb =
  header
    [ h1 [] [ text "Greek Conjugator" ]
    , input
      [ placeholder "What verb would you like to conjugate?"
      , autofocus true
      , value verb
      , name "verb"
      , onInput UpdateField
      , onEnter ConjugateField
      ]
      []
    ]


  