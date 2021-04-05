module Types.Class exposing (Class(..), toString)


type Class
    = Lower
    | Middle
    | Upper
    | Elite


toString : Class -> String
toString class =
    case class of
        Lower ->
            "Lower"

        Middle ->
            "Middle"

        Upper ->
            "Upper"

        Elite ->
            "Elite"
