module Wealth exposing (Model, Msg, init, update, view)

import Browser.Dom as Dom
import Character.Types exposing (Character)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Json
import Session
import Task


type Msg
    = NoOp
    | Editing Int String Bool
    | UpdateWealth Int String String


type alias Model =
    { session : Session.Data
    , editing : Maybe ( Int, String )
    }


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( Model session Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Editing characterId name isEditing ->
            let
                editing =
                    if isEditing then
                        Just ( characterId, name )

                    else
                        Nothing

                focus =
                    Dom.focus (String.fromInt characterId ++ "-wealth-" ++ name)
            in
            ( { model | editing = editing }, Task.attempt (\_ -> NoOp) focus )

        UpdateWealth characterId name value ->
            let
                val =
                    Maybe.withDefault 3 (String.toInt value)

                session =
                    model.session

                newWealth wealth =
                    case name of
                        "platinum" ->
                            { wealth | platinum = val }

                        "gold" ->
                            { wealth | gold = val }

                        "silver" ->
                            { wealth | silver = val }

                        "electrum" ->
                            { wealth | electrum = val }

                        "copper" ->
                            { wealth | copper = val }

                        _ ->
                            wealth

                updateWealth character =
                    { character | wealth = newWealth character.wealth }

                updateCharacter character =
                    if character.id == characterId then
                        updateWealth character

                    else
                        character

                updatedSession =
                    { session | characters = List.map updateCharacter session.characters }
            in
            ( { model | session = updatedSession }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "wealth" ]
        [ table [ class "table is-bordered" ]
            [ thead []
                [ tr []
                    [ th []
                        []
                    , th []
                        [ text "Platinum" ]
                    , th []
                        [ text "Electrum" ]
                    , th []
                        [ text "Gold" ]
                    , th []
                        [ text "Silver" ]
                    , th []
                        [ text "Copper" ]
                    ]
                ]
            , tbody []
                (List.map (viewCharacter model) model.session.characters)
            ]
        ]


viewCharacter : Model -> Character -> Html Msg
viewCharacter model character =
    tr
        []
        [ td [] [ text character.name ]
        , td [] [ item model "platinum" character.id character.wealth.platinum ]
        , td [] [ item model "electrum" character.id character.wealth.electrum ]
        , td [] [ item model "gold" character.id character.wealth.gold ]
        , td [] [ item model "silver" character.id character.wealth.silver ]
        , td [] [ item model "copper" character.id character.wealth.copper ]
        ]


item : Model -> String -> Int -> Int -> Html Msg
item model name characterId amt =
    let
        className x y =
            if x == characterId && y == name then
                "editing"

            else
                ""

        klass =
            case model.editing of
                Just ( a, b ) ->
                    className a b

                Nothing ->
                    ""
    in
    div [ class klass ]
        [ label [ onClick (Editing characterId name True) ] [ text (String.fromInt amt) ]
        , input
            [ id (String.fromInt characterId ++ "-wealth-" ++ name)
            , value (String.fromInt amt)
            , onInput (UpdateWealth characterId name)
            , onBlur (Editing characterId name False)
            , onEnter (Editing characterId name False)
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)
