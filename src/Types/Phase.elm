module Types.Phase exposing (Phase(..), toString)


type Phase
    = Birth
    | Childhood
    | Teens
    | EarlyAdulthood
    | AdultHood
    | LateAdulthood
    | Elder


toString : Phase -> String
toString phase =
    case phase of
        Birth ->
            "Birth"

        Childhood ->
            "Childhood"

        Teens ->
            "Teens"

        EarlyAdulthood ->
            "Early Adulthood"

        AdultHood ->
            "AdultHood"

        LateAdulthood ->
            "Late Adulthood"

        Elder ->
            "Elder"
