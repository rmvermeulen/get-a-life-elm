module Main exposing (..)

import Browser
import Colors.Opaque as Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    column
        [ Background.color Colors.white
        , padding 10
        , Border.width 1
        , Border.shadow
            { blur = 4
            , color = Colors.black
            , offset = ( 2, 1 )
            , size = 1
            }
        ]
        [ text "App is running!"
        , text <| "model: " ++ Debug.toString model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view =
            view
                >> layout
                    [ padding 10
                    , Border.solid
                    , Background.color Colors.aliceblue
                    ]
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
