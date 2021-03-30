module Main exposing (..)

import Browser
import Colors.Opaque as Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Random
import Random.List



---- GENERATORS ----


genBirthplace : Random.Generator Birthplace
genBirthplace =
    Random.List.choose
        [ Europe
        , NorthAmerica
        , SouthAmerica
        , Afrika
        , Asia
        , Australia
        ]
        |> Random.map (\( mChoice, rest ) -> Maybe.withDefault Europe mChoice)



---- MODEL ----


type Class
    = Lower
    | Middle
    | Upper
    | Elite


type SkinColor
    = White
    | Brown
    | Black


type Birthplace
    = Europe
    | NorthAmerica
    | SouthAmerica
    | Afrika
    | Asia
    | Australia


type Profile
    = Empty
    | Step1 Birthplace
    | Step2 Birthplace SkinColor
    | Step3 Birthplace SkinColor Class


type alias Model =
    { profile : Profile
    }


init : ( Model, Cmd Msg )
init =
    ( Model Empty
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetProfile Profile
    | GenBirthplace


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        simply m =
            ( m, Cmd.none )
    in
    case msg of
        SetProfile profile ->
            simply { model | profile = profile }

        GenBirthplace ->
            ( model
            , Random.generate (Step1 >> SetProfile) genBirthplace
            )



---- VIEW ----


view : Model -> Element Msg
view model =
    column
        [ centerX
        , Background.color Colors.white
        , padding 10
        , Border.width 1
        , Border.shadow
            { blur = 4
            , color = Colors.black
            , offset = ( 2, 1 )
            , size = 1
            }
        , spacing 8
        , width (fillPortion 3)
        ]
        [ "debug: "
            ++ Debug.toString model
            |> text
            |> el
                [ Font.italic
                , Background.color Colors.grey
                , padding 8
                , Font.color Colors.white
                , Border.rounded 4
                ]
        , row [ width fill ]
            [ viewProfile model.profile
            , Input.button
                [ Border.width 1
                , Border.color Colors.red
                , Font.color Colors.red
                , padding 8
                , alignRight
                ]
                { label = text "Reset"
                , onPress = Just <| SetProfile Empty
                }
            ]
        ]


viewProfile : Profile -> Element Msg
viewProfile profile =
    case profile of
        Empty ->
            column []
                [ text "Let put together a lifetime of stuff!"
                , Input.button
                    [ Border.width 1, padding 8 ]
                    { label = text "Get a life!"
                    , onPress = Just GenBirthplace
                    }
                ]

        _ ->
            text "...todo..."



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view =
            view
                >> layout
                    [ padding 18
                    , Border.solid
                    , Background.color Colors.aliceblue
                    ]
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
