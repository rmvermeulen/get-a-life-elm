module Types.Life exposing (..)

import Types.Phase as Phase exposing (Phase)
import Types.Profile as Profile exposing (Profile)


type Life
    = Life Profile Phase


toString : Life -> String
toString life =
    case life of
        Life profile phase ->
            "[Life " ++ Profile.toString profile ++ "," ++ Phase.toString phase "]"
