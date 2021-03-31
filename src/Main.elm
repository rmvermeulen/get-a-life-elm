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



---- HELPERS ----


type alias Weighted value =
    ( Float, value )


type alias NonEmptyList value =
    ( value, List value )



---- DATA ----


getWeightedSkinColors : Birthplace -> NonEmptyList (Weighted SkinColor)
getWeightedSkinColors birthplace =
    case birthplace of
        Europe ->
            ( ( 90, White )
            , [ ( 10, Brown )
              , ( 8, Black )
              ]
            )

        NorthAmerica ->
            ( ( 90, White )
            , [ ( 10, Brown )
              , ( 8, Black )
              ]
            )

        SouthAmerica ->
            ( ( 90, Brown )
            , [ ( 25, White )
              , ( 20, Black )
              ]
            )

        Afrika ->
            ( ( 90, Black )
            , [ ( 10, Brown )
              , ( 8, White )
              ]
            )

        Asia ->
            ( ( 90, Brown )
            , [ ( 10, White )
              , ( 10, Black )
              ]
            )

        Australia ->
            ( ( 90, White )
            , [ ( 10, Brown )
              , ( 30, Black )
              ]
            )



---- GENERATORS ----


randomPick : NonEmptyList item -> Random.Generator item
randomPick ( head, rest ) =
    let
        list =
            head :: rest
    in
    Random.List.choose list
        |> Random.map (Tuple.first >> Maybe.withDefault head)


genBirthplace : Random.Generator Birthplace
genBirthplace =
    randomPick
        ( Europe
        , [ NorthAmerica
          , SouthAmerica
          , Afrika
          , Asia
          , Australia
          ]
        )


genSkinColor : Birthplace -> Random.Generator SkinColor
genSkinColor birthplace =
    let
        ( head, rest ) =
            getWeightedSkinColors birthplace
    in
    Random.weighted head rest



-- randomPick White [ Brown, Black ]


genClass : Birthplace -> SkinColor -> Random.Generator Class
genClass birthplace skinColor =
    randomPick ( Lower, [ Middle, Upper, Elite ] )


genBirthYear : ( Int, Int ) -> Random.Generator BirthYear
genBirthYear ( from, to ) =
    Random.int from to



---- MODEL ----


type alias BirthYear =
    Int


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
    | Step4 Birthplace SkinColor Class Int
    | FinalStep Summary



-- profileName : Profile -> String
-- profileName profile =case profile of


type alias Settings =
    { yearRange : ( Int, Int )
    }


type alias Model =
    { profile : Profile
    , settings : Settings
    }


init : ( Model, Cmd Msg )
init =
    ( Model Empty (Settings ( 1900, 2020 ))
    , Cmd.none
    )



---- UPDATE ----


type alias Summary =
    { birthplace : Birthplace, skinColor : SkinColor, class : Class, year : Int }


type Msg
    = SetProfile Profile
    | GenStep1
    | GenStep2
    | GenStep3
    | GenStep4
    | GenSummary


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

        GenStep4 ->
            case model.profile of
                Step3 birthplace skinColor class ->
                    ( model
                    , Random.generate (Step4 birthplace skinColor class >> SetProfile) (genBirthYear model.settings.yearRange)
                    )

                _ ->
                    simply model

        GenSummary ->
            case model.profile of
                Step4 birthplace skinColor class year ->
                    simply { model | profile = FinalStep <| Summary birthplace skinColor class year }

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
                    { label = text "find out"
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
                    { label = text "See"
                    , onPress = Just GenStep3
                    }
                ]

        Step3 birthplace skinColor class ->
            column [ spacing 12 ]
                [ text <|
                    "Right in the "
                        ++ (String.toLower <| Debug.toString class)
                        ++ " class, that'll be something!"
                , text "So, when is all this going on?"
                , plainButton []
                    { label = text "Find out"
                    , onPress = Just GenStep4
                    }
                ]

        Step4 birthplace skinColor class year ->
            column [ spacing 12 ]
                [ text <| String.fromInt year
                , text "How exciting!"
                , plainButton []
                    { label = text "View summary"
                    , onPress = Just GenSummary
                    }
                ]

        FinalStep summary ->
            text <| Debug.toString summary



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
