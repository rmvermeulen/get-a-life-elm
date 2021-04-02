module AnimatedButton exposing (Model, Msg, init, subscriptions, update, view)

import Animator
import Color
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
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
        maybeAlways value =
            Maybe.map (\_ -> value)

        setButtonState id newState =
            Dict.update id (maybeAlways newState) <| Animator.current model.states

        simply m =
            ( m, Cmd.none )
    in
    case msg of
        RuntimeTriggeredAnimationStep delta ->
            simply <| Animator.update delta animator model

        ButtonHoverStart id ->
            simply <|
                { model
                    | states =
                        Animator.go Animator.slowly
                            (setButtonState id Hover)
                            model.states
                }

        ButtonHoverEnd id ->
            simply <|
                { model
                    | states =
                        Animator.go Animator.slowly
                            (setButtonState id Default)
                            model.states
                }


view : Model -> Element Msg
view model =
    let
        buttonState id =
            Maybe.withDefault Default <| Dict.get id <| Animator.current model.states

        borderColor id =
            E.fromRgb <|
                Color.toRgba <|
                    if buttonState id == Hover then
                        Color.blue

                    else
                        Color.black

        fontColor id =
            E.fromRgb <|
                Color.toRgba <|
                    if buttonState id == Hover then
                        Color.white

                    else
                        Color.black

        bgColor id =
            E.fromRgb <|
                Color.toRgba <|
                    Animator.color model.states <|
                        \states ->
                            if (Maybe.withDefault Default <| Dict.get id states) == Hover then
                                Color.lightBlue

                            else
                                Color.white

        fontSize id =
            round <|
                Animator.linear model.states <|
                    \states ->
                        Animator.at <|
                            if (Maybe.withDefault Default <| Dict.get id states) == Hover then
                                28

                            else
                                20

        button id =
            E.el
                [ E.width <| E.px 200
                , E.height <| E.px 60
                , Border.width 3
                , Border.rounded 6
                , Border.color <| borderColor id
                , Background.color <| bgColor id
                , Font.color <| fontColor id
                , Font.size <| fontSize id
                , E.padding 10
                , Events.onMouseEnter <| ButtonHoverStart id
                , Events.onMouseLeave <| ButtonHoverEnd id
                ]
            <|
                (E.el [ E.centerX, E.centerY ] <| E.text <| "Button " ++ id)
    in
    [ "Uno", "Dos", "Tres" ]
        |> List.map button
        |> E.column [ E.spacing 10, E.centerX, E.centerY ]


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
