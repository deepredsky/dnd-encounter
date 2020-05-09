module Encounter exposing
    ( Model
    , Msg
    , encounterDecoder
    , encounterToValue
    , init
    , update
    , view
    )

import Encounter.Types exposing (Encounter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
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


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ section
            [ class "content" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-third" ] [ viewEncounters model ]
                , div [ class "column" ] [ text "form here" ]
                ]
            ]
        ]


viewEncounters : Model -> Html Msg
viewEncounters model =
    let
        encounters =
            model.session.encounters
    in
    div []
        [ h1 [] [ text "Encounters" ]
        , Keyed.node "table" [ class "table is-bordered" ] <|
            List.map viewKeyedEncounter encounters
        ]


viewKeyedEncounter : Encounter -> ( String, Html Msg )
viewKeyedEncounter encounter =
    ( String.fromInt encounter.id, lazy viewEncounter encounter )


viewEncounter encounter =
    tr
        []
        [ td [] [ text encounter.name ]
        ]


encounterDecoder : Decode.Decoder Encounter
encounterDecoder =
    Decode.map4 Encounter
        (field "characters" (Decode.list Decode.int))
        (field "name" Decode.string)
        (field "current" Decode.int)
        (field "id" Decode.int)


encounterToValue encounter =
    Encode.object
        [ ( "characters", Encode.list Encode.int encounter.characters )
        , ( "name", Encode.string encounter.name )
        , ( "current", Encode.int encounter.current )
        ]
