module Encounter.Types exposing (Encounter)


type alias Encounter =
    { characters : List Int
    , name : String
    , current : Int
    }
