port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Json
import Task

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = \model -> { title = "Greek Conjugator", body = [view model] }
    , update = update
    , subscriptions = \_ -> Sub.none
    }



-- MODEL  


type alias Model =
  { field : String 
  , tenses : List Tense
  , currentTense : String
  , conjugation : List (String, String)
  }

type alias Tense =
  { name : String
  , isChecked : Bool
  , msg : Msg
  }

emptyModel : Model 
emptyModel =
  { field = ""
  , tenses = 
    [ { name = "ενεστώτας", isChecked = True, msg = (ChangeTense "ενεστώτας" ) }
    , { name = "αόριστος", isChecked = False, msg = (ChangeTense "αόριστος" ) }
    , { name = "μέλλοντας", isChecked = False, msg = (ChangeTense "μέλλοντας" ) }  
    ]
  , currentTense = "ενεστώτας"
  , conjugation = []
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( emptyModel 
  , Cmd.none
  )



-- UPDATE

type Msg
  = NoOp
  | UpdateField String
  | ChangeTense String
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
          | conjugation =
            if String.endsWith "άω" model.field then
              if model.currentTense == "ενεστώτας" then
                [ ("εγώ", model.field)
                , ("εσύ", String.dropRight 1 model.field ++ "ς")
                , ("αυτός", String.dropRight 1 model.field ++ "ει")
                , ("εμείς", String.dropRight 1 model.field ++ "με")
                , ("εσείς", String.dropRight 1 model.field ++ "τε")
                , ("αυτοί", String.dropRight 1 model.field ++ "ν(ε)")                  
                ]         
              else
                [ ("εγώ", String.dropRight 1 model.field ++ "ήσω")
                , ("εσύ", String.dropRight 1 model.field ++ "ήσεις")
                , ("αυτός", String.dropRight 2 model.field ++ "ήσει")
                , ("εμείς", String.dropRight 2 model.field ++ "ήσουμε")
                , ("εσείς", String.dropRight 2 model.field ++ "ήσετε")
                , ("αυτοί", String.dropRight 2 model.field ++ "ήσουν(ε)")                  
                ]                
            
            else if String.endsWith "ώ" model.field then
              if model.currentTense == "ενεστώτας" then
                [ ("εγώ", model.field)
                , ("εσύ", String.dropRight 1 model.field ++ "είς")
                , ("αυτός", String.dropRight 1 model.field ++ "εί")
                , ("εμείς", String.dropRight 1 model.field ++ "ούμε")
                , ("εσείς", String.dropRight 1 model.field ++ "είτε")
                , ("αυτοί", String.dropRight 1 model.field ++ "ούν(ε)")
                ]              
              else
                [ ("εγώ", String.dropRight 1 model.field ++ "ήσω")
                , ("εσύ", String.dropRight 1 model.field ++ "ήσεις")
                , ("αυτός", String.dropRight 1 model.field ++ "ήσει")
                , ("εμείς", String.dropRight 1 model.field ++ "ήσουμε")
                , ("εσείς", String.dropRight 1 model.field ++ "ήσετε")
                , ("αυτοί", String.dropRight 1 model.field ++ "ήσουν(ε)")                  
                ]  

            else if String.endsWith "ω" model.field then
              [ ("εγώ", model.field)
              , ("εσύ", String.dropRight 1 model.field ++ "εις")
              , ("αυτός", String.dropRight 1 model.field ++ "ει")
              , ("εμείς", String.dropRight 1 model.field ++ "ουμε")
              , ("εσείς", String.dropRight 1 model.field ++ "ετε")
              , ("αυτοί", String.dropRight 1 model.field ++ "ουν(ε)")
              ] 
              
            else
              [ ("Sorry,", "that's not a valid verb.")]
        }
        , Cmd.none
      )
        
    ChangeTense tense ->
      let
        updateTense tenseToUpdate =
          if tenseToUpdate.name == tense then
            { tenseToUpdate | isChecked = True }
          else
            { tenseToUpdate | isChecked = False }
      in
      ( { model 
        | currentTense = tense
        , tenses = List.map updateTense model.tenses
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
      , lazy viewTenses model.tenses
      ]
    ]

viewInput : String -> Html Msg
viewInput verb =
  header []
    [ h1 [] [ text "Greek Conjugator" ]
    , div [ class "verb-search" ]
      [ input
        [  placeholder "What verb would you like to conjugate?"
        , autofocus True
        , value verb
        , name "verb"
        , onInput UpdateField
        , onEnter ConjugateField
        ]
        []
      ]
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

viewTenses : List Tense -> Html Msg
viewTenses tenses =
  div [ class "controls" ]
  [ fieldset [ class "controls" ] <|
    List.map radio tenses
  ]

radio : Tense -> Html Msg
radio tense = 
  label []
    [ input [ type_ "radio", name tense.name, onClick tense.msg, checked tense.isChecked ] []
    , text tense.name
    ]

viewConjugations : List (String, String) -> Html Msg
viewConjugations conjugation =
  section []
  [ div [ class "conjugation" ] <|
    List.map viewConj conjugation
  ]

viewConj : (String, String) -> Html Msg
viewConj conjTuple =
  p [] [ text (Tuple.first conjTuple ++ " " ++ Tuple.second conjTuple) ]