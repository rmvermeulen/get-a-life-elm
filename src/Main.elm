module Main exposing (..)

import AnimatedButton
import Animation
import Browser
import Colors.Opaque as Colors
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Random
import Random.List
import Types.Birth as Birth exposing (Birth)
import Types.BodyInfo as BodyInfo exposing (BodyInfo(..))
import Types.Class as Class exposing (Class(..))
import Types.Place exposing (Place(..))
import Types.Profile exposing (Profile(..))
import Types.Settings exposing (Settings)
import Types.Year exposing (Year)



---- HELPERS ----


getBoxWidth : Profile -> Float
getBoxWidth profile =
    case profile of
        Complete _ ->
            300

        Partial { mBirth, mClass, mBodyInfo } ->
            [ mBirth |> Maybe.map Birth.toString
            , mClass |> Maybe.map Class.toString
            , mBodyInfo |> Maybe.map BodyInfo.toString
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


getBoxHeight : Profile -> Float
getBoxHeight profile =
    case profile of
        Complete _ ->
            160

        Partial { mBirth, mClass, mBodyInfo } ->
            let
                count =
                    Maybe.map (always 1) >> Maybe.withDefault 0

                sum =
                    [ mBirth |> count
                    , mClass |> count
                    , mBodyInfo |> count
                    ]
                        |> List.foldl (+) 0
            in
            50 + 30 * sum


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
        , mBodyInfo = Nothing
        , mClass = Nothing
        }



---- DATA ----


getWeightedBodyInfos : Place -> NonEmptyList (Weighted BodyInfo)
getWeightedBodyInfos birthplace =
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


genBodyInfo : Place -> Year -> Random.Generator BodyInfo
genBodyInfo birthplace _ =
    let
        ( head, rest ) =
            getWeightedBodyInfos birthplace
    in
    Random.weighted head rest


genBirth : Settings -> Random.Generator Birth
genBirth { yearRange } =
    Random.map2 Birth genPlace (genYear yearRange)


genClass : Place -> BodyInfo -> Random.Generator Class
genClass _ _ =
    randomPick ( Class.Lower, [ Class.Middle, Class.Upper, Class.Elite ] )


genYear : ( Int, Int ) -> Random.Generator Year
genYear ( from, to ) =
    Random.int from to



---- MODEL ----


type alias Model =
    { profile : Profile
    , settings : Settings
    , button : AnimatedButton.Model
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

        ( button, abCmd ) =
            AnimatedButton.init
                [ AnimatedButton.ButtonConfig "birth" "Be born!" ]

        style =
            Animation.style
                [ Animation.width (Animation.px 400)
                , Animation.height (Animation.px 100)
                ]
    in
    ( Model
        emptyPartialProfile
        settings
        button
        style
    , Cmd.batch [ abCmd |> Cmd.map AnimatedButtonMsg ]
    )



---- UPDATE ----


type Msg
    = SetProfile Profile
    | GenBirth
    | GenBodyInfo Birth
    | GenClass Birth BodyInfo
    | SetBirth Birth
    | SetBodyInfo BodyInfo
    | SetClass Class
    | CompleteProfile
    | AnimatedButtonMsg AnimatedButton.Msg


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
                    , Animation.height <|
                        Animation.px <|
                            getBoxHeight profile
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
        GenBodyInfo { place, year } ->
            generate (genBodyInfo place year) SetBodyInfo

        GenClass { place } bodyInfo ->
            generate (genClass place bodyInfo) SetClass

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

        SetBodyInfo bodyInfo ->
            let
                profile =
                    case model.profile of
                        Complete summary ->
                            Complete { summary | bodyInfo = bodyInfo }

                        Partial summary ->
                            Partial { summary | mBodyInfo = Just bodyInfo }
            in
            simply
                { model
                    | profile = profile
                }

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
                Partial { mBirth, mBodyInfo, mClass } ->
                    case ( mBirth, mBodyInfo, mClass ) of
                        ( Just birth, Just bodyInfo, Just class ) ->
                            simply
                                { model
                                    | profile =
                                        Complete
                                            { birth = birth
                                            , bodyInfo = bodyInfo
                                            , class = class
                                            }
                                }

                        _ ->
                            simply model

                _ ->
                    simply model

        AnimatedButtonMsg abMsg ->
            let
                ( newModel, cmd1 ) =
                    case abMsg of
                        AnimatedButton.ButtonPressed id ->
                            let
                                myMsg =
                                    case id of
                                        "birth" ->
                                            GenBirth

                                        _ ->
                                            Debug.todo <| "Unimplemented Button id '" ++ id ++ "'"
                            in
                            update myMsg model

                        _ ->
                            simply model

                ( button, cmd2 ) =
                    AnimatedButton.update abMsg model.button
            in
            ( { newModel | button = button }
            , Cmd.batch
                [ cmd1
                , Cmd.map
                    AnimatedButtonMsg
                    cmd2
                ]
            )



---- VIEW ----


view : Model -> Element Msg
view model =
    E.column
        [ E.centerX
        , E.alignTop
        , Background.color Colors.white
        , E.padding 12
        , Border.width 1
        , Border.shadow
            { blur = 4
            , color = Colors.black
            , offset = ( 2, 1 )
            , size = 1
            }
        , E.spacing 16
        , E.width E.shrink
        ]
        [ viewProfile model
        ]


viewProfile : Model -> Element Msg
viewProfile { profile, button } =
    let
        plainButton attrs obj =
            Input.button
                ([ Border.width 1, E.padding 8 ] ++ attrs)
                obj
    in
    case profile of
        Partial { mBirth, mBodyInfo, mClass } ->
            let
                viewClass birth bodyInfo =
                    case mClass of
                        Nothing ->
                            -- { label = E.text "What class am I in?"
                            -- , onPress = Just <| GenClass birth bodyInfo
                            -- }
                            AnimatedButton.view button
                                |> E.map AnimatedButtonMsg

                        Just class ->
                            E.column [ E.spacing 12 ]
                                [ E.text <| Class.toString class ++ " class"
                                , plainButton
                                    []
                                    { label = E.text "Summarize"
                                    , onPress = Just CompleteProfile
                                    }
                                ]

                viewBodyInfo birth =
                    case mBodyInfo of
                        Nothing ->
                            AnimatedButton.view button
                                |> E.map AnimatedButtonMsg

                        Just bodyInfo ->
                            E.column [ E.spacing 12 ]
                                [ E.text <| BodyInfo.toString bodyInfo
                                , viewClass birth bodyInfo
                                ]

                viewBirth =
                    case mBirth of
                        Nothing ->
                            E.column [ E.spacing 12 ]
                                [ E.text "Let put together a lifetime of stuff!"
                                , AnimatedButton.view button
                                    |> E.map AnimatedButtonMsg
                                ]

                        Just birth ->
                            E.column [ E.spacing 12 ]
                                [ E.text <| Birth.toString birth
                                , viewBodyInfo birth
                                ]
            in
            viewBirth

        Complete { birth, class, bodyInfo } ->
            E.column []
                [ E.text "Summary:"
                , birth |> Birth.toString |> E.text
                , class |> Class.toString |> E.text
                , bodyInfo |> BodyInfo.toString |> E.text
                ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimatedButton.subscriptions model.button
            |> Sub.map AnimatedButtonMsg
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view =
            view
                >> E.layout
                    [ E.padding 18
                    , Border.solid
                    , Background.color Colors.aliceblue
                    ]
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
