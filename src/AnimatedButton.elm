module AnimatedButton exposing (Msg, State)

import Animator
import Dict exposing (Dict)
import Time


type alias Id =
    String


type State
    = Default
    | Hover
    | Pressed


type Msg
    = RuntimeTriggeredAnimationStep Time.Posix
    | ButtonHoverStart Id
    | ButtonHoverEnd Id


type alias Model =
    { states : Animator.Timeline (Dict Id State)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { states =
                Animator.init <|
                    Dict.fromList
                        [ ( "One", Default )
                        , ( "Two", Default )
                        , ( "Three", Default )
                        ]
            }
    in
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        simply m =
            ( m, Cmd.none )
    in
    case msg of
        RuntimeTriggeredAnimationStep delta ->
            simply model

        _ ->
            simply model
