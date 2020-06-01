module Character.Types exposing (Character, Stat, Wealth)


type alias Character =
    { id : Int
    , name : String
    , stat : Stat
    , wealth : Wealth
    }


type alias Wealth =
    { gold : Int
    , silver : Int
    , copper : Int
    , platinum : Int
    , electrum : Int
    }


type alias Stat =
    { hitPoints : Int
    , armour : Int
    , initiative : Int
    }
