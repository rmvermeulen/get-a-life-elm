module Tests exposing (..)

import Expect
import Test exposing (..)
import Types.Birth exposing (Birth)
import Types.BodyInfo as BodyInfo
import Types.Class as Class exposing (Class(..))
import Types.Phase as Phase exposing (Phase)
import Types.Place as Place exposing (Place(..))
import Types.Profile as Profile exposing (Profile(..))



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


types : Test
types =
    describe "toString methods of data types"
        [ describe "Types.Profile"
            [ test "toString: empty partial" <|
                \_ ->
                    let
                        p =
                            Profile.Partial
                                { mBirth = Nothing
                                , mBodyInfo = Nothing
                                , mClass = Nothing
                                }

                        result =
                            Profile.toString p

                        expected =
                            "[Profile:Partial {}]"
                    in
                    Expect.equal result expected
            , test "toString: partial with class" <|
                \_ ->
                    let
                        p =
                            Profile.Partial
                                { mBirth = Nothing
                                , mBodyInfo = Nothing
                                , mClass = Just Class.Lower
                                }

                        result =
                            Profile.toString p

                        expected =
                            "[Profile:Partial {Class=Lower}]"
                    in
                    Expect.equal result expected
            , test "toString: partial with birth, class" <|
                \_ ->
                    let
                        p =
                            Profile.Partial
                                { mBirth = Just <| Birth Place.Afrika 2000
                                , mBodyInfo = Nothing
                                , mClass = Just Class.Lower
                                }

                        result =
                            Profile.toString p

                        expected =
                            "[Profile:Partial {Birth=Afrika, 2000 Class=Lower}]"
                    in
                    Expect.equal result expected
            , test "toString: partial with everything" <|
                \_ ->
                    let
                        p =
                            Profile.Partial
                                { mBirth = Just <| Birth Place.Afrika 2000
                                , mBodyInfo = Just <| BodyInfo.White
                                , mClass = Just Class.Lower
                                }

                        result =
                            Profile.toString p

                        expected =
                            "[Profile:Partial {Birth=Afrika, 2000 BodyInfo=White Class=Lower}]"
                    in
                    Expect.equal result expected
            , test "toString: Complete" <|
                \_ ->
                    let
                        p =
                            Profile.Complete
                                { birth = Birth Place.Australia 2000
                                , bodyInfo = BodyInfo.Black
                                , class = Class.Lower
                                }

                        result =
                            Profile.toString p

                        expected =
                            "[Profile:Complete {Birth=Australia, 2000 BodyInfo=Black Class=Lower}]"
                    in
                    Expect.equal result expected
            ]
        , describe "Phase"
            [ describe "toString"
                [ test "Birth" <| \_ -> Expect.equal "Birth" <| Phase.toString Phase.Birth
                , test "Childhood" <| \_ -> Expect.equal "Childhood" <| Phase.toString Phase.Childhood
                , test "Teens" <| \_ -> Expect.equal "Teens" <| Phase.toString Phase.Teens
                , test "Early Adulthood" <| \_ -> Expect.equal "Early Adulthood" <| Phase.toString Phase.EarlyAdulthood
                , test "Adulthood" <| \_ -> Expect.equal "Adulthood" <| Phase.toString Phase.Adulthood
                , test "Late Adulthood" <| \_ -> Expect.equal "Late Adulthood" <| Phase.toString Phase.LateAdulthood
                , test "Elder" <| \_ -> Expect.equal "Elder" <| Phase.toString Phase.Elder
                ]
            ]
        ]
