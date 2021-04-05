module Types.Phase exposing (Phase(..), toString)


type Phase
    = Birth
    | Childhood
    | Teens
    | EarlyAdulthood
    | Adulthood
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

        Adulthood ->
            "Adulthood"

        LateAdulthood ->
            "Late Adulthood"

        Elder ->
            "Elder"
