module Types.Year exposing (Year, toString)


type alias Year =
    Int


toString : Year -> String
toString year =
    String.fromInt year
