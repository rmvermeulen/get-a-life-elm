module Types.Profile exposing (Profile(..), toString)

import Types.Birth as Birth exposing (Birth)
import Types.BodyInfo as BodyInfo exposing (BodyInfo)
import Types.Class as Class exposing (Class)


type Profile
    = Partial
        { mBirth : Maybe Birth
        , mBodyInfo : Maybe BodyInfo
        , mClass : Maybe Class
        }
    | Complete
        { birth : Birth
        , bodyInfo : BodyInfo
        , class : Class
        }


toString : Profile -> String
toString profile =
    let
        m2s s f =
            Maybe.map (f >> (++) s)

        bs mBirth =
            m2s "Birth=" Birth.toString mBirth

        bis mBodyInfo =
            m2s "BodyInfo=" BodyInfo.toString mBodyInfo

        cs mClass =
            m2s "Class=" Class.toString mClass

        summary ms =
            ms
                |> List.filterMap identity
                |> String.join " "
    in
    case profile of
        Partial { mBirth, mBodyInfo, mClass } ->
            let
                content =
                    summary
                        [ bs mBirth
                        , bis mBodyInfo
                        , cs mClass
                        ]
            in
            "[Profile:Partial {" ++ content ++ "}]"

        Complete { birth, bodyInfo, class } ->
            let
                content =
                    summary
                        [ bs <| Just birth
                        , bis <| Just bodyInfo
                        , cs <| Just class
                        ]
            in
            "[Profile:Complete {" ++ content ++ "}]"
