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
    | UpdateNameField String
    | Add


type alias Model =
    { session : Session.Data
    , form : Form
    }


type alias Form =
    { name : String }


emptyForm : Form
emptyForm =
    { name = "" }


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( { session = session, form = emptyForm }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            let
                session =
                    model.session

                encounters =
                    session.encounters

                newSession =
                    { session | encounters = encounters ++ [ Encounter [] model.form.name 1 1 ] }
            in
            ( { model | form = emptyForm, session = newSession }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        UpdateNameField name ->
            let
                form =
                    model.form

                newForm =
                    { form | name = name }
            in
            ( { model | form = newForm }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ section
            [ class "content" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-third" ] [ viewEncounters model ]
                , div [ class "column" ] [ viewForm model ]
                ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    let
        form =
            model.form
    in
    Html.form [ onSubmit Add ]
        [ div [ class "field" ]
            [ label [ class "label" ] [ text "Name" ]
            , input
                [ class "input input-lg"
                , placeholder "Name"
                , autofocus True
                , onInput UpdateNameField
                , value form.name
                ]
                []
            ]
        , button [ class "button is-primary" ]
            [ text "Add" ]
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


viewEncounter : Encounter -> Html Msg
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


encounterToValue : Encounter -> Encode.Value
encounterToValue encounter =
    Encode.object
        [ ( "characters", Encode.list Encode.int encounter.characters )
        , ( "name", Encode.string encounter.name )
        , ( "current", Encode.int encounter.current )
        ]
