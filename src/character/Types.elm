module Character.Types exposing (Character)


type alias Character =
    { name : String
    , hit_points : Int
    , armour : Int
    , initiative : Int
    , id : Int
    }
