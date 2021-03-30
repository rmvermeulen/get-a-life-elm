module Main exposing (..)

import Browser
import Element exposing (..)



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
    column [] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> layout []
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
