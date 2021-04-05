module Types.Birth exposing (Birth, toString)

import Types.Place as Place exposing (Place)
import Types.Year as Year exposing (Year)


type alias Birth =
    { place : Place, year : Year }


toString : Birth -> String
toString { place, year } =
    Place.toString place ++ ", " ++ Year.toString year
