import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)

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

type Verb
  = GroupA String
  | GroupB String
  | GroupG String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )


    UpdateField str ->
      ( { model | field = str }
      , Cmd.none
      )

    
    ConjugateField verb ->
      case verb of

        GroupA str ->
          ( { model | conjugation =
            { str
            , String.dropRight 1 ++ "εις"
            , String.dropRight 1 ++ "ει"
            , String.dropRight 1 ++ "ουμε"
            , String.dropRight 1 ++ "ετε"
            , String.dropRight 1 ++ "ουνε"
            } }
            , Cmd.none
          )


        GroupB str ->
          ( { model | conjugation = 
            { str
            , String.dropRight 1 str ++ "ς"
            , String.dropRight 1 str ++ "ει"
            , String.dropRight 1 str ++ "με"
            , String.dropRight 1 str ++ "τε"
            , String.dropRight 1 str ++ "νε"
            } }
            , Cmd.none
          })


        GroupG str ->
          ( { model | conjugation = 
            { str
            , String.dropRight 1 str ++ "είς"
            , String.dropRight 1 str ++ "εί"
            , String.dropRight 1 str ++ "ούμε"
            , String.dropRight 1 str ++ "είτε"
            , String.dropRight 1 str ++ "ούνε"
            } }
            , Cmd.none
          })
        
