module Character exposing
    ( Character
    , Form
    , characterDecoder
    , characterToValue
    , emptyCharacter
    , emptyCharacterForm
    , formDecoder
    , newCharacter
    , newCharacterForm
    )

import Html exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode


type alias Character =
    { name : String
    , hit_points : Int
    , armour : Int
    , initiative : Int
    , id : Int
    }


type alias Form =
    { name : String
    , hit_points : Int
    , armour : Int
    , initiative : Int
    }


newCharacterForm =
    { name = ""
    , hit_points = 0
    , armour = 0
    , initiative = 0
    }


newCharacter : Form -> Int -> Character
newCharacter form id =
    { name = form.name
    , id = id
    , armour = form.armour
    , hit_points = form.hit_points
    , initiative = form.initiative
    }


characterDecoder : Decode.Decoder Character
characterDecoder =
    Decode.map5 Character
        (field "name" Decode.string)
        (field "armour" Decode.int)
        (field "hit_points" Decode.int)
        (field "initiative" Decode.int)
        (field "id" Decode.int)


formDecoder : Decode.Decoder Form
formDecoder =
    Decode.map4 Form
        (field "name" Decode.string)
        (field "armour" Decode.int)
        (field "hit_points" Decode.int)
        (field "initiative" Decode.int)


characterToValue character =
    Encode.object
        [ ( "name", Encode.string character.name )
        , ( "hit_points", Encode.int character.hit_points )
        , ( "armour", Encode.int character.armour )
        , ( "initiative", Encode.int character.initiative )
        , ( "id", Encode.int character.id )
        ]


emptyCharacter =
    { name = ""
    , id = 0
    , armour = 0
    , hit_points = 0
    , initiative = 0
    }


emptyCharacterForm =
    { name = ""
    , hit_points = 0
    , armour = 0
    , initiative = 0
    }
