module Types.BodyInfo exposing (BodyInfo(..), toString)


type BodyInfo
    = White
    | Brown
    | Black


toString : BodyInfo -> String
toString skin =
    case skin of
        White ->
            "White"

        Brown ->
            "Brown"

        Black ->
            "Black"
