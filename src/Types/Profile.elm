module Types.Profile exposing (Profile(..))

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
