module AnimatedButton exposing (Model, Msg, init, subscriptions, update, view)

import Animator
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Input as Input
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        RuntimeTriggeredAnimationStep delta ->
            model

        ButtonHoverStart id ->
            model

        ButtonHoverEnd id ->
            model


view : Model -> Element Msg
view model =
    Input.button []
        { label = E.text "animated button"
        , onPress = Nothing
        }


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watchingWith .states
            (\newStates model ->
                { model | states = newStates }
            )
            (\states -> List.any ((==) Hover) <| Dict.values states)


subscriptions : Model -> Sub Msg
subscriptions model =
    Animator.toSubscription RuntimeTriggeredAnimationStep model animator
