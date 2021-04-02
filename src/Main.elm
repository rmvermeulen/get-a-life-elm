module Main exposing (..)

import Animation
import Browser
import Colors.Opaque as Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Random
import Random.List



---- HELPERS ----


classToString : Class -> String
classToString class =
    case class of
        Lower ->
            "Lower"

        Middle ->
            "Middle"

        Upper ->
            "Upper"

        Elite ->
            "Elite"


skinColorToString : SkinColor -> String
skinColorToString skin =
    case skin of
        White ->
            "White"

        Brown ->
            "Brown"

        Black ->
            "Black"


placeToString : Place -> String
placeToString place =
    case place of
        Afrika ->
            "Afrika"

        Asia ->
            "Asia"

        Australia ->
            "Australia"

        Europe ->
            "Europe"

        NorthAmerica ->
            "North America"

        SouthAmerica ->
            "South America"


yearToString : Year -> String
yearToString year =
    String.fromInt year


birthToString : Birth -> String
birthToString { place, year } =
    placeToString place ++ ", " ++ yearToString year


getBoxWidth : Profile -> Float
getBoxWidth profile =
    case profile of
        Complete _ ->
            200

        Partial { mBirth, mClass, mSkinColor } ->
            [ mBirth |> Maybe.map birthToString
            , mClass |> Maybe.map classToString
            , mSkinColor |> Maybe.map skinColorToString
            ]
                -- find the longest string's length
                |> List.filterMap
                    (Maybe.map <| String.length >> toFloat)
                |> (List.sort >> List.reverse >> List.head)
                |> Maybe.withDefault 10
                -- character width, roughly
                |> (*) 12
                -- add double the padding
                |> (+) 32


type alias Weighted value =
    ( Float, value )


type alias NonEmptyList value =
    ( value, List value )


collapse : Result t t -> t
collapse result =
    case result of
        Ok value ->
            value

        Err value ->
            value


emptyPartialProfile : Profile
emptyPartialProfile =
    Partial
        { mBirth = Nothing
        , mSkinColor = Nothing
        , mClass = Nothing
        }



---- DATA ----


getWeightedSkinColors : Place -> NonEmptyList (Weighted SkinColor)
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


genPlace : Random.Generator Place
genPlace =
    randomPick
        ( Europe
        , [ NorthAmerica
          , SouthAmerica
          , Afrika
          , Asia
          , Australia
          ]
        )


genSkinColor : Place -> Year -> Random.Generator SkinColor
genSkinColor birthplace _ =
    let
        ( head, rest ) =
            getWeightedSkinColors birthplace
    in
    Random.weighted head rest


genBirth : Settings -> Random.Generator Birth
genBirth { yearRange } =
    Random.map2 Birth genPlace (genYear yearRange)


genClass : Place -> SkinColor -> Random.Generator Class
genClass _ _ =
    randomPick ( Lower, [ Middle, Upper, Elite ] )


genYear : ( Int, Int ) -> Random.Generator Year
genYear ( from, to ) =
    Random.int from to



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


type Place
    = Europe
    | NorthAmerica
    | SouthAmerica
    | Afrika
    | Asia
    | Australia


type alias Year =
    Int


type alias Birth =
    { place : Place, year : Year }


type Profile
    = Partial
        { mBirth : Maybe Birth
        , mSkinColor : Maybe SkinColor
        , mClass : Maybe Class
        }
    | Complete
        { birth : Birth
        , skinColor : SkinColor
        , class : Class
        }



-- profileName : Profile -> String
-- profileName profile =case profile of


type alias Settings =
    { yearRange : ( Int, Int )
    , previewEnabled : Bool
    , json :
        { indent : Int
        , columns : Int
        }
    }


type alias Model =
    { profile : Profile
    , settings : Settings
    , style : Animation.State
    }


init : ( Model, Cmd Msg )
init =
    let
        settings =
            Settings
                ( 1900, 2020 )
                True
                { indent = 2, columns = 120 }

        style =
            Animation.style
                [ Animation.width (Animation.px 200)
                ]
    in
    ( Model
        emptyPartialProfile
        settings
        style
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetProfile Profile
    | GenBirth
    | GenSkinColor Birth
    | GenClass Birth SkinColor
    | SetBirth Birth
    | SetSkinColor SkinColor
    | SetClass Class
    | CompleteProfile
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        perform c =
            ( model, c )

        generate g m =
            perform <| Random.generate m g

        simply m =
            ( m, Cmd.none )

        animateBox profile =
            Animation.interrupt
                [ Animation.to
                    [ Animation.width <|
                        Animation.px <|
                            getBoxWidth profile
                    ]
                ]
                model.style
    in
    case msg of
        SetProfile profile ->
            simply { model | profile = profile }

        GenBirth ->
            generate (genBirth model.settings) SetBirth

        -- GenBirthplace ->
        -- GenYear ->
        GenSkinColor { place, year } ->
            generate (genSkinColor place year) SetSkinColor

        GenClass { place } skinColor ->
            generate (genClass place skinColor) SetClass

        SetBirth birth ->
            let
                profile =
                    case model.profile of
                        Complete summary ->
                            Complete { summary | birth = birth }

                        Partial summary ->
                            Partial { summary | mBirth = Just birth }

                style =
                    animateBox profile
            in
            simply { model | profile = profile, style = style }

        SetSkinColor skinColor ->
            let
                profile =
                    case model.profile of
                        Complete summary ->
                            Complete { summary | skinColor = skinColor }

                        Partial summary ->
                            Partial { summary | mSkinColor = Just skinColor }

                style =
                    animateBox profile
            in
            simply
                { model
                    | profile = profile
                    , style = style
                }

        SetClass class ->
            let
                profile =
                    case model.profile of
                        Complete summary ->
                            Complete { summary | class = class }

                        Partial summary ->
                            Partial { summary | mClass = Just class }

                style =
                    animateBox profile
            in
            simply { model | profile = profile, style = style }

        CompleteProfile ->
            case model.profile of
                Partial { mBirth, mSkinColor, mClass } ->
                    case ( mBirth, mSkinColor, mClass ) of
                        ( Just birth, Just skinColor, Just class ) ->
                            simply
                                { model
                                    | profile =
                                        Complete
                                            { birth = birth
                                            , skinColor = skinColor
                                            , class = class
                                            }
                                }

                        _ ->
                            simply model

                _ ->
                    simply model

        Animate animMsg ->
            simply { model | style = Animation.update animMsg model.style }



---- VIEW ----


view : Model -> Element Msg
view model =
    let
        animations =
            Animation.render model.style
                |> List.map htmlAttribute
                |> List.map (mapAttribute Animate)
    in
    viewProfile model.profile
        |> el
            (animations
                ++ [ centerX
                   , alignTop
                   , Background.color Colors.white
                   , padding 12
                   , Border.width 1
                   , Border.shadow
                        { blur = 4
                        , color = Colors.black
                        , offset = ( 2, 1 )
                        , size = 1
                        }
                   , spacing 16
                   , width shrink
                   ]
            )


viewProfile : Profile -> Element Msg
viewProfile profile =
    let
        plainButton attrs obj =
            Input.button
                ([ Border.width 1, padding 8 ] ++ attrs)
                obj
    in
    case profile of
        Partial { mBirth, mSkinColor, mClass } ->
            let
                viewClass birth skinColor =
                    case mClass of
                        Nothing ->
                            plainButton
                                []
                                { label = text "What class am I in?"
                                , onPress = Just <| GenClass birth skinColor
                                }

                        Just class ->
                            column [ spacing 12 ]
                                [ text <| classToString class ++ " class"
                                , plainButton
                                    []
                                    { label = text "Summarize"
                                    , onPress = Just CompleteProfile
                                    }
                                ]

                viewSkinColor birth =
                    case mSkinColor of
                        Nothing ->
                            plainButton
                                []
                                { label = text "What do I look like?"
                                , onPress = Just <| GenSkinColor birth
                                }

                        Just skinColor ->
                            column [ spacing 12 ]
                                [ text <| skinColorToString skinColor
                                , viewClass birth skinColor
                                ]

                viewBirth =
                    case mBirth of
                        Nothing ->
                            column [ spacing 12 ]
                                [ text "Let put together a lifetime of stuff!"
                                , plainButton
                                    []
                                    { label = text "Get a life!"
                                    , onPress = Just GenBirth
                                    }
                                ]

                        Just birth ->
                            column [ spacing 12 ]
                                [ text <| birthToString birth
                                , viewSkinColor birth
                                ]
            in
            viewBirth

        Complete { birth, class, skinColor } ->
            column []
                [ text "Summary:"
                , birth |> birthToString |> text
                , class |> classToString |> text
                , skinColor |> skinColorToString |> text
                ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]



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
        , subscriptions = subscriptions
        }
