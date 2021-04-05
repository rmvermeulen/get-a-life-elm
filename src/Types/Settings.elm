module Types.Settings exposing (Settings)


type alias Settings =
    { yearRange : ( Int, Int )
    , previewEnabled : Bool
    , json :
        { indent : Int
        , columns : Int
        }
    }
