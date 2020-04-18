module Encounter exposing
    ( Encounter
    , encounterDecoder
    , encounterToValue
    )

import Json.Decode as Decode exposing (field)
import Json.Encode as Encode


type alias Encounter =
    { characters : List Int
    , name : String
    , current : Int
    }


encounterDecoder : Decode.Decoder Encounter
encounterDecoder =
    Decode.map3 Encounter
        (field "characters" (Decode.list Decode.int))
        (field "name" Decode.string)
        (field "current" Decode.int)


encounterToValue encounter =
    Encode.object
        [ ( "characters", Encode.list Encode.int encounter.characters )
        , ( "name", Encode.string encounter.name )
        , ( "current", Encode.int encounter.current )
        ]
