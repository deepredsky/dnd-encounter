port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Character
    exposing
        ( Model
        , Msg
        , characterDecoder
        , characterToValue
        , emptyCharacter
        , emptyCharacterForm
        , formDecoder
        , init
        , newCharacter
        , update
        , view
        )
import Encounter
    exposing
        ( Encounter
        , encounterDecoder
        , encounterToValue
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Json exposing (andThen, field)
import Json.Encode as E
import Session exposing (Data, emptyData)
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)



-- main : Program (Maybe E.Value) Model Msg


main =
    Browser.application
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


port setStorage : E.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model

        session =
            sessionFromModel newModel
    in
    ( newModel
    , Cmd.batch [ setStorage (sessionToValue session), cmds ]
    )


sessionFromModel : Model -> Session.Data
sessionFromModel model =
    case model.page of
        TestPage session ->
            session

        NotFound session ->
            session

        Character m ->
            m.session



-- MODEL


type alias Model =
    { page : Page
    }


type Page
    = NotFound Session.Data
    | TestPage Session.Data
    | Character Character.Model


emptyModel : Model
emptyModel =
    { page = NotFound Session.emptyData
    }


init : Maybe E.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init savedModel url _ =
    let
        session value =
            Maybe.withDefault Session.emptyData (Json.decodeValue sessionDecoder value |> resultToMaybe)

        model =
            case savedModel of
                Just value ->
                    { page = NotFound (session value)
                    }

                _ ->
                    emptyModel
    in
    stepUrl url model



-- UPDATE


type Msg
    = NoOp
    | CharacterMsg Character.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged url ->
            stepUrl url model

        CharacterMsg msg ->
            case model.page of
                Character data ->
                    stepSearch model (Character.update msg data)

                _ ->
                    ( model, Cmd.none )


stepSearch : Model -> ( Character.Model, Cmd Character.Msg ) -> ( Model, Cmd Msg )
stepSearch model ( data, cmds ) =
    ( { model | page = Character data }
    , Cmd.map CharacterMsg cmds
    )


exit : Model -> Session.Data
exit model =
    case model.page of
        TestPage session ->
            session

        NotFound session ->
            session

        Character m ->
            m.session


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            exit model

        parser =
            oneOf
                [ route top
                    (stepSearch model (Character.init session))
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound session }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        TestPage _ ->
            { title = "oeuoeu"
            , body =
                [ div [] [ text "wtf" ]
                ]
            }

        NotFound _ ->
            { title = "404"
            , body =
                [ div [] [ text "404 not found" ]
                ]
            }

        Character d ->
            { title = "Characters"
            , body =
                [ Html.map CharacterMsg (Character.view d)
                ]
            }


viewFooter : Html msg
viewFooter =
    footer [ class "footer" ]
        [ div [ class "content has-text-centered" ]
            [ p []
                [ strong []
                    [ text "D&D Encounter App" ]
                ]
            , p []
                [ text "Written by "
                , a [ href "https://github.com/deepredsky" ]
                    [ text "Rajesh Sharma" ]
                ]
            ]
        ]


sessionToValue session =
    E.object
        [ ( "characters", E.list characterToValue session.characters )
        , ( "encounters", E.list encounterToValue session.encounters )
        , ( "uid", E.int session.uid )
        ]


sessionDecoder =
    Json.map3 Session.Data
        (field "encounters" (Json.list encounterDecoder))
        (field "characters" (Json.list characterDecoder))
        (field "uid" Json.int)


resultToMaybe result =
    case result of
        Result.Ok model ->
            Just model

        Result.Err error ->
            -- Debug.log (Json.errorToString error) Nothing
            Nothing
