module Wealth exposing (Model, Msg, init, update, view)

import Browser.Dom as Dom
import Character exposing (idToCharacter)
import Character.Types exposing (Character)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Session
import Task


type Msg
    = NoOp
    | UpdatePlatinumField String
    | UpdateElectrumField String
    | UpdateGoldField String
    | UpdateSilverField String
    | UpdateCopperField String
    | Editing Int String Bool
    | UpdateWealth Int String String
    | SelectCharacter Int Bool
    | DistributeWealth


type alias Model =
    { session : Session.Data
    , editing : Maybe ( Int, String )
    , selectedCharacters : List Int
    , form : Form
    }


type alias Form =
    { platinum : String
    , electrum : String
    , gold : String
    , silver : String
    , copper : String
    }


emptyForm : Form
emptyForm =
    { platinum = "0"
    , electrum = "0"
    , gold = "0"
    , silver = "0"
    , copper = "0"
    }


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( Model session Nothing [] emptyForm, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DistributeWealth ->
            let
                session =
                    model.session

                countSelected =
                    List.length model.selectedCharacters

                share wealth =
                    Maybe.withDefault 0 (String.toInt wealth) // countSelected

                newWealth wealth =
                    { wealth
                        | copper = wealth.copper + share model.form.copper
                        , silver = wealth.silver + share model.form.silver
                        , gold = wealth.gold + share model.form.gold
                        , platinum = wealth.platinum + share model.form.platinum
                        , electrum = wealth.electrum + share model.form.electrum
                    }

                updateWealth character =
                    { character | wealth = newWealth character.wealth }

                updateCharacter character =
                    if List.member character.id model.selectedCharacters then
                        updateWealth character

                    else
                        character

                updatedSession =
                    { session | characters = List.map updateCharacter session.characters }
            in
            ( { model | session = updatedSession, form = emptyForm }, Cmd.none )

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
                    Maybe.withDefault 0 (String.toInt value)

                session =
                    model.session

                newWealth wealth =
                    case name of
                        "platinum" ->
                            { wealth | platinum = val }

                        "electrum" ->
                            { wealth | electrum = val }

                        "gold" ->
                            { wealth | gold = val }

                        "silver" ->
                            { wealth | silver = val }

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

        SelectCharacter id selected ->
            let
                selectedCharacters =
                    if selected then
                        model.selectedCharacters ++ [ id ]

                    else
                        List.filter (\t -> t /= id) model.selectedCharacters
            in
            ( { model | selectedCharacters = selectedCharacters }, Cmd.none )

        UpdatePlatinumField value ->
            let
                form =
                    model.form

                newForm =
                    { form | platinum = value }
            in
            ( { model | form = newForm }, Cmd.none )

        UpdateElectrumField value ->
            let
                form =
                    model.form

                newForm =
                    { form | electrum = value }
            in
            ( { model | form = newForm }, Cmd.none )

        UpdateGoldField value ->
            let
                form =
                    model.form

                newForm =
                    { form | gold = value }
            in
            ( { model | form = newForm }, Cmd.none )

        UpdateSilverField value ->
            let
                form =
                    model.form

                newForm =
                    { form | silver = value }
            in
            ( { model | form = newForm }, Cmd.none )

        UpdateCopperField value ->
            let
                form =
                    model.form

                newForm =
                    { form | copper = value }
            in
            ( { model | form = newForm }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.form [ onSubmit DistributeWealth ]
        [ div [ class "wealth" ]
            [ table [ class "table is-bordered" ]
                [ thead []
                    [ tr []
                        [ th [] []
                        , th []
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
                , tfoot []
                    (viewSelectedCharacters model)
                ]
            ]
        ]


viewCharacter : Model -> Character -> Html Msg
viewCharacter model character =
    tr
        []
        [ td [] [ input [ type_ "checkbox", onCheck (SelectCharacter character.id) ] [] ]
        , td [] [ text character.name ]
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
            , type_ "number"
            , onInput (UpdateWealth characterId name)
            , onBlur (Editing characterId name False)
            , onEnter (Editing characterId name False)
            ]
            []
        ]


viewSelectedCharacters : Model -> List (Html Msg)
viewSelectedCharacters model =
    if List.isEmpty model.selectedCharacters then
        [ text "" ]

    else
        [ tr []
            [ td [ colspan 2 ]
                [ input
                    [ type_ "submit"
                    , value "Distribute Wealth"
                    ]
                    []
                ]
            , td []
                [ input
                    [ name "platinum"
                    , placeholder "Platinum"
                    , onInput UpdatePlatinumField
                    , value model.form.platinum
                    ]
                    []
                ]
            , td []
                [ input
                    [ name "electrum"
                    , placeholder "Electrum"
                    , onInput UpdateElectrumField
                    , value model.form.electrum
                    ]
                    []
                ]
            , td []
                [ input
                    [ name "gold"
                    , placeholder "Gold"
                    , onInput UpdateGoldField
                    , value model.form.gold
                    ]
                    []
                ]
            , td []
                [ input
                    [ name "silver"
                    , placeholder "Silver"
                    , onInput UpdateSilverField
                    , value model.form.silver
                    ]
                    []
                ]
            , td []
                [ input
                    [ name "copper"
                    , placeholder "Copper"
                    , onInput UpdateCopperField
                    , value model.form.copper
                    ]
                    []
                ]
            ]
        , tr []
            [ td [ colspan 7 ]
                [ div [ class "tags" ] <| List.map (viewSelectedCharacter model) model.selectedCharacters
                ]
            ]
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


viewSelectedCharacter : Model -> Int -> Html Msg
viewSelectedCharacter model selectedCharactersId =
    let
        maybeCharacter =
            idToCharacter model.session.characters (Just selectedCharactersId)
    in
    case maybeCharacter of
        Just char ->
            span [ class "tag is-small is-success" ] [ text char.name ]

        Nothing ->
            span [] []
