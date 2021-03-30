module Main exposing (..)

import Array exposing (empty)
import Browser
import Colors.Opaque as Colors
import Debug exposing (toString)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Random
import Random.List



---- GENERATORS ----


randomPick : item -> List item -> Random.Generator item
randomPick head rest =
    let
        list =
            head :: rest
    in
    Random.List.choose list
        |> Random.map (Tuple.first >> Maybe.withDefault head)


genBirthplace : Random.Generator Birthplace
genBirthplace =
    randomPick Europe
        [ NorthAmerica
        , SouthAmerica
        , Afrika
        , Asia
        , Australia
        ]


genSkinColor : Birthplace -> Random.Generator SkinColor
genSkinColor birthplace =
    randomPick White [ Brown, Black ]


genClass : Birthplace -> SkinColor -> Random.Generator Class
genClass birthplace skinColor =
    randomPick Lower [ Middle, Upper, Elite ]



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



-- profileName : Profile -> String
-- profileName profile =case profile of


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
    | GenStep1
    | GenStep2
    | GenStep3


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        simply m =
            ( m, Cmd.none )
    in
    case msg of
        SetProfile profile ->
            simply { model | profile = profile }

        GenStep1 ->
            case model.profile of
                Empty ->
                    ( model
                    , Random.generate (Step1 >> SetProfile) genBirthplace
                    )

                _ ->
                    simply model

        GenStep2 ->
            case model.profile of
                Step1 birthplace ->
                    ( model
                    , Random.generate (Step2 birthplace >> SetProfile) (genSkinColor birthplace)
                    )

                _ ->
                    simply model

        GenStep3 ->
            case model.profile of
                Step2 birthplace skinColor ->
                    ( model
                    , Random.generate (Step3 birthplace skinColor >> SetProfile) (genClass birthplace skinColor)
                    )

                _ ->
                    simply model



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
        , spacing 16
        , width (fillPortion 3)
        ]
        [ row []
            [ let
                ( color, action ) =
                    if model.profile == Empty then
                        -- make the button 'disabled'
                        ( Colors.gray, Nothing )

                    else
                        ( Colors.red, Just (SetProfile Empty) )
              in
              Input.button
                [ Border.width 1
                , Border.color color
                , Font.color color
                , padding 8
                ]
                { label = text "Reset"
                , onPress = action
                }
            , "debug: "
                ++ Debug.toString model
                |> text
                |> el
                    [ Font.italic
                    , Background.color Colors.grey
                    , padding 8
                    , Font.color Colors.white
                    , Border.rounded 4
                    ]
            ]
        , viewProfile model.profile
        ]


viewProfile : Profile -> Element Msg
viewProfile profile =
    let
        plainButton attrs obj =
            Input.button
                ([ Border.width 1, padding 8 ] ++ attrs)
                obj
    in
    case profile of
        Empty ->
            column [ spacing 12 ]
                [ text "Let put together a lifetime of stuff!"
                , plainButton
                    []
                    { label = text "Get a life!"
                    , onPress = Just GenStep1
                    }
                ]

        Step1 birthplace ->
            column [ spacing 12 ]
                [ text <| Debug.toString birthplace ++ ", interesting!"
                , text "Let's find out what you're made of!"
                , plainButton
                    []
                    { label = text "Let's find out!"
                    , onPress = Just GenStep2
                    }
                ]

        Step2 birthplace skinColor ->
            column [ spacing 12 ]
                [ text <|
                    "Wow, "
                        ++ (String.toLower <| Debug.toString skinColor)
                        ++ " skin, in "
                        ++ Debug.toString birthplace
                        ++ "! Very tasteful."
                , text "Let's see what your material conditions might be!"
                , plainButton []
                    { label = text "Yes, please!"
                    , onPress = Just GenStep3
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
