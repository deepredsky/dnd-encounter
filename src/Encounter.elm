module Encounter exposing
    ( Model
    , Msg
    , encounterDecoder
    , encounterToValue
    , init
    , update
    )

import Encounter.Types exposing (Encounter)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode
import Session


type Msg
    = NoOp


type alias Model =
    { session : Session.Data
    }


init session =
    ( { session = session }, Cmd.none )


update msg data =
    ( data, Cmd.none )


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
