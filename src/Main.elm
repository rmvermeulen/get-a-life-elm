module Main exposing (..)

import Browser
import Colors.Opaque as Colors
import DebugToJson
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Print
import Random
import Random.List



---- HELPERS ----


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
genSkinColor birthplace year =
    let
        ( head, rest ) =
            getWeightedSkinColors birthplace
    in
    Random.weighted head rest


genBirth : Settings -> Random.Generator Birth
genBirth { yearRange } =
    Random.map2 Birth genPlace (genYear yearRange)


genClass : Place -> SkinColor -> Random.Generator Class
genClass place skinColor =
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
    }


init : ( Model, Cmd Msg )
init =
    let
        settings =
            Settings
                ( 1900, 2020 )
                True
                { indent = 2, columns = 120 }
    in
    ( Model
        emptyPartialProfile
        settings
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
    | SetJsonIndent Int
    | SetJsonColumns Int
    | ShowModel
    | HideModel
    | SetModelPreviewEnabled Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        perform c =
            ( model, c )

        generate g m =
            perform <| Random.generate m g

        simply m =
            ( m, Cmd.none )
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

        GenClass { place, year } skinColor ->
            generate (genClass place skinColor) SetClass

        SetBirth birth ->
            let
                profile =
                    case model.profile of
                        Complete summary ->
                            Complete { summary | birth = birth }

                        Partial summary ->
                            Partial { summary | mBirth = Just birth }
            in
            simply { model | profile = profile }

        SetSkinColor skinColor ->
            let
                profile =
                    case model.profile of
                        Complete summary ->
                            Complete { summary | skinColor = skinColor }

                        Partial summary ->
                            Partial { summary | mSkinColor = Just skinColor }
            in
            simply { model | profile = profile }

        SetClass class ->
            let
                profile =
                    case model.profile of
                        Complete summary ->
                            Complete { summary | class = class }

                        Partial summary ->
                            Partial { summary | mClass = Just class }
            in
            simply { model | profile = profile }

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

        SetJsonIndent indent ->
            let
                newSettings =
                    let
                        { settings } =
                            model
                    in
                    { settings
                        | json =
                            { indent = indent
                            , columns = settings.json.columns
                            }
                    }
            in
            simply { model | settings = newSettings }

        SetJsonColumns columns ->
            let
                newSettings =
                    let
                        { settings } =
                            model
                    in
                    { settings
                        | json =
                            { columns = columns
                            , indent = settings.json.indent
                            }
                    }
            in
            simply { model | settings = newSettings }

        ShowModel ->
            update (SetModelPreviewEnabled True) model

        HideModel ->
            update (SetModelPreviewEnabled False) model

        SetModelPreviewEnabled enabled ->
            let
                { settings } =
                    model

                newSettings =
                    { settings | previewEnabled = enabled }
            in
            simply { model | settings = newSettings }



---- VIEW ----


view : Model -> Element Msg
view model =
    viewProfile model.profile
        |> el
            [ centerX
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


viewDebugStuff : Model -> Element Msg
viewDebugStuff model =
    let
        { profile, settings } =
            model

        resetButton =
            let
                ( color, action ) =
                    case profile of
                        Complete _ ->
                            -- make the button 'disabled'
                            ( Colors.gray, Nothing )

                        Partial _ ->
                            ( Colors.red, Just (SetProfile emptyPartialProfile) )
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
    in
    el
        [ Background.color Colors.darkYellow2
        , padding 12
        ]
    <|
        if settings.previewEnabled then
            column []
                [ row []
                    [ resetButton
                    , Input.button
                        []
                        { label = text "Hide model"
                        , onPress = Just HideModel
                        }
                    ]
                , let
                    json : String
                    json =
                        model
                            |> Debug.toString
                            |> DebugToJson.toJson
                            |> Result.map
                                (Json.Print.prettyValue
                                    settings.json
                                    >> collapse
                                )
                            |> Result.withDefault "oops"

                    -- |> prettyString { indent = 2, columns = 4 }
                    -- |> collapse
                  in
                  json
                    |> text
                    |> el
                        [ Font.italic
                        , Font.alignLeft
                        , Font.color Colors.white
                        , Background.color Colors.grey
                        , padding 8
                        , Border.rounded 4
                        , width shrink
                        ]
                , Input.slider [ padding 10 ]
                    { label = Input.labelLeft [] <| text "indent"
                    , max = 12
                    , min = 0
                    , onChange = Basics.floor >> SetJsonIndent
                    , step = Just 1
                    , thumb = Input.defaultThumb
                    , value = toFloat settings.json.indent
                    }
                , Input.slider [ padding 10 ]
                    { label = Input.labelLeft [] <| text "columns"
                    , max = 500
                    , min = 0
                    , onChange = Basics.floor >> SetJsonColumns
                    , step = Just 1
                    , thumb = Input.defaultThumb
                    , value = toFloat settings.json.columns
                    }
                ]

        else
            row []
                [ resetButton
                , Input.button []
                    { label = text "Show model"
                    , onPress = Just ShowModel
                    }
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
                                [ text <| Debug.toString class ++ " class"
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
                                [ text <| Debug.toString skinColor
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
                            let
                                ps =
                                    Debug.toString birth.place

                                ys =
                                    String.fromInt birth.year
                            in
                            column [ spacing 12 ]
                                [ text <| ps ++ ", " ++ ys
                                , viewSkinColor birth
                                ]
            in
            viewBirth

        Complete { birth } ->
            text "Summary:"



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
