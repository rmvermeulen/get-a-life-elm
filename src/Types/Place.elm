module Types.Place exposing (Place(..), toString)


type Place
    = Europe
    | NorthAmerica
    | SouthAmerica
    | Afrika
    | Asia
    | Australia


toString : Place -> String
toString place =
    case place of
        Afrika ->
            "Afrika"

        Asia ->
            "Asia"

        Australia ->
            "Australia"

        Europe ->
            "Europe"

        NorthAmerica ->
            "North America"

        SouthAmerica ->
            "South America"
