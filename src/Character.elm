module Character exposing
    ( Form
    , Model
    , Msg
    , characterDecoder
    , characterToValue
    , emptyCharacter
    , emptyCharacterForm
    , idToCharacter
    , init
    , newCharacter
    , newCharacterForm
    , update
    , view
    , viewForm
    , viewSelectedCharacter
    )

import Character.Types exposing (Character, Stat, Wealth)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode exposing (andThen, field)
import Json.Encode as Encode
import Random
import Session


type alias Form =
    { name : String
    , hitPoints : Int
    , armour : Int
    , initiative : Int
    , editing : EditingState
    }


type alias Model =
    { session : Session.Data
    , form : Form
    , selectedCharacters : List Int
    , selectedCharacter : Maybe Int
    }


type EditingState
    = Editing Int
    | NotEditing


newCharacterForm : Form
newCharacterForm =
    { name = ""
    , hitPoints = 0
    , armour = 0
    , initiative = 0
    , editing = NotEditing
    }


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( Model session newCharacterForm [] Nothing, Cmd.none )


newCharacter : Form -> Int -> Character
newCharacter form id =
    { name = form.name
    , id = id
    , stat = newStat form
    , wealth = emptyWealth
    }


newStat : Form -> Stat
newStat form =
    { armour = form.armour
    , hitPoints = form.hitPoints
    , initiative = form.initiative
    }


characterDecoder : Decode.Decoder Character
characterDecoder =
    Decode.map4 Character
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "stat" statDecoder)
        (field "wealth" wealthDecoder)


statDecoder : Decode.Decoder Stat
statDecoder =
    Decode.map3 Stat
        (field "armour" Decode.int)
        (field "hitPoints" Decode.int)
        (field "initiative" Decode.int)


wealthDecoder : Decode.Decoder Wealth
wealthDecoder =
    Decode.map5 Wealth
        (field "gold" Decode.int)
        (field "silver" Decode.int)
        (field "copper" Decode.int)
        (field "platinum" Decode.int)
        (field "electrum" Decode.int)


characterToValue : Character -> Encode.Value
characterToValue character =
    Encode.object
        [ ( "id", Encode.int character.id )
        , ( "name", Encode.string character.name )
        , ( "stat", statToValue character.stat )
        , ( "wealth", wealthToValue character.wealth )
        ]


statToValue : Stat -> Encode.Value
statToValue stat =
    Encode.object
        [ ( "hitPoints", Encode.int stat.hitPoints )
        , ( "armour", Encode.int stat.armour )
        , ( "initiative", Encode.int stat.initiative )
        ]


wealthToValue : Wealth -> Encode.Value
wealthToValue wealth =
    Encode.object
        [ ( "gold", Encode.int wealth.gold )
        , ( "silver", Encode.int wealth.silver )
        , ( "copper", Encode.int wealth.copper )
        , ( "platinum", Encode.int wealth.platinum )
        , ( "electrum", Encode.int wealth.electrum )
        ]


emptyCharacter : Character
emptyCharacter =
    { name = ""
    , id = 0
    , stat = emptyStat
    , wealth = emptyWealth
    }


emptyStat : Stat
emptyStat =
    { hitPoints = 0
    , initiative = 0
    , armour = 0
    }


emptyWealth : Wealth
emptyWealth =
    { gold = 0
    , silver = 0
    , copper = 0
    , platinum = 0
    , electrum = 0
    }


emptyCharacterForm : Form
emptyCharacterForm =
    { name = ""
    , hitPoints = 0
    , armour = 0
    , initiative = 0
    , editing = NotEditing
    }


saveCharacter : Model -> Int -> List Character
saveCharacter model id =
    let
        updatedStat form =
            { armour = form.armour
            , hitPoints = form.hitPoints
            , initiative = form.initiative
            }

        save character =
            if character.id == id then
                { name = model.form.name
                , id = id
                , stat = updatedStat model.form
                , wealth = emptyWealth
                }

            else
                character
    in
    List.map save model.session.characters


type Msg
    = NoOp
    | UpdateNameField String
    | UpdateInitiativeField String
    | UpdateArmourField String
    | UpdateHitPointsField String
    | EditingCharacter Int
    | UpdateCharacter Int String
    | Add
    | Delete Int
    | SelectCharacter Int Bool
    | SelectRandomCharacter
    | NewSelectRandomCharacter Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add ->
            let
                session =
                    model.session

                characters =
                    if String.isEmpty model.form.name then
                        session.characters

                    else
                        case model.form.editing of
                            NotEditing ->
                                session.characters ++ [ newCharacter model.form session.uid ]

                            Editing id ->
                                saveCharacter model id

                newSession =
                    { session
                        | uid = model.session.uid + 1
                        , characters = characters
                    }
            in
            ( { model
                | session = newSession
                , form = emptyCharacterForm
              }
            , Cmd.none
            )

        UpdateNameField str ->
            updateForm (\form -> { form | name = str }) model

        UpdateHitPointsField str ->
            let
                newHitPoint =
                    Maybe.withDefault 0 (String.toInt str)
            in
            updateForm (\form -> { form | hitPoints = newHitPoint }) model

        UpdateArmourField str ->
            let
                newHitPoint =
                    Maybe.withDefault 0 (String.toInt str)
            in
            updateForm (\form -> { form | armour = newHitPoint }) model

        UpdateInitiativeField str ->
            let
                newHitPoint =
                    Maybe.withDefault 0 (String.toInt str)
            in
            updateForm (\form -> { form | initiative = newHitPoint }) model

        EditingCharacter id ->
            let
                maybeCharacter =
                    List.head (List.filter (\c -> c.id == id) model.session.characters)

                character =
                    Maybe.withDefault emptyCharacter maybeCharacter

                session =
                    model.session

                newSession =
                    { session | uid = id }
            in
            ( { model
                | session = newSession
                , form =
                    { name = character.name
                    , hitPoints = character.stat.hitPoints
                    , armour = character.stat.armour
                    , initiative = character.stat.initiative
                    , editing = Editing id
                    }
              }
            , Cmd.none
            )

        UpdateCharacter id task ->
            let
                updateCharacter t =
                    if t.id == id then
                        { t | name = task }

                    else
                        t

                session =
                    model.session

                newSession =
                    { session | characters = List.map updateCharacter session.characters }
            in
            ( { model | session = newSession }
            , Cmd.none
            )

        Delete id ->
            let
                session =
                    model.session

                newSession =
                    { session | characters = List.filter (\t -> t.id /= id) session.characters }
            in
            ( { model | session = newSession }
            , Cmd.none
            )

        SelectCharacter id selected ->
            let
                selectedCharacters =
                    if selected then
                        model.selectedCharacters ++ [ id ]

                    else
                        List.filter (\t -> t /= id) model.selectedCharacters
            in
            ( { model | selectedCharacters = selectedCharacters }, Cmd.none )

        SelectRandomCharacter ->
            ( model, Random.generate NewSelectRandomCharacter (Random.int 0 (List.length model.selectedCharacters)) )

        NewSelectRandomCharacter idx ->
            ( { model | selectedCharacter = itemAt idx model.selectedCharacters }, Cmd.none )


itemAt : Int -> List a -> Maybe a
itemAt idx items =
    List.head <| List.reverse (List.take (idx + 1) items)


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ section
            [ class "content" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-third" ] [ viewForm model ]
                , div [ class "column" ]
                    [ div [] [ lazy viewCharacters model.session.characters ]
                    , div [] [ viewSelectedCharacters model ]
                    ]
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
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Hit Points" ]
            , input
                [ class "input input-lg"
                , placeholder "Hit Points"
                , onInput UpdateHitPointsField
                , value (String.fromInt form.hitPoints)
                ]
                []
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Armour" ]
            , input
                [ class "input input-lg"
                , placeholder "Armour"
                , onInput UpdateArmourField
                , value (String.fromInt form.armour)
                ]
                []
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Initiative" ]
            , input
                [ class "input input-lg"
                , placeholder "Initiative"
                , onInput UpdateInitiativeField
                , value (String.fromInt form.initiative)
                ]
                []
            ]
        , button [ class "button is-primary" ]
            [ text <| buttonText model.form.editing ]
        ]


viewKeyedCharacter : Character -> ( String, Html Msg )
viewKeyedCharacter character =
    ( String.fromInt character.id, lazy viewCharacter character )


viewCharacter : Character -> Html Msg
viewCharacter character =
    tr
        []
        [ td [] [ input [ type_ "checkbox", onCheck (SelectCharacter character.id) ] [] ]
        , td [] [ text character.name ]
        , td [] [ text (String.fromInt character.stat.hitPoints) ]
        , td [] [ text (String.fromInt character.stat.armour) ]
        , td [] [ text (String.fromInt character.stat.initiative) ]
        , td []
            [ button
                [ onClick (Delete character.id)
                ]
                [ text "Delete" ]
            , button
                [ onClick (EditingCharacter character.id)
                ]
                [ text "Edit" ]
            ]
        ]


viewCharacters : List Character -> Html Msg
viewCharacters characters =
    section
        [ class "main" ]
        [ h1 [] [ text "Characters" ]
        , Keyed.node "table" [ class "table is-bordered" ] <|
            List.map viewKeyedCharacter characters
        ]


viewSelectedCharacters : Model -> Html Msg
viewSelectedCharacters model =
    if List.isEmpty model.selectedCharacters then
        div [] []

    else
        div []
            [ hr [] []
            , div [ class "tags" ] <| List.map (viewSelectedCharacter model) model.selectedCharacters
            , div [] [ button [ onClick SelectRandomCharacter ] [ text "Choose One" ] ]
            , div [] [ text <| viewRandomlySelectedCharacter (idToCharacter model.session.characters model.selectedCharacter) ]
            ]


viewRandomlySelectedCharacter : Maybe Character -> String
viewRandomlySelectedCharacter maybeCharacter =
    let
        character =
            Maybe.withDefault emptyCharacter maybeCharacter
    in
    character.name


idToCharacter : List Character -> Maybe Int -> Maybe Character
idToCharacter characters id =
    id
        |> Maybe.andThen
            (\num -> List.head <| List.filter (\character -> character.id == num) characters)


viewSelectedCharacter : Model -> Int -> Html Msg
viewSelectedCharacter model selectedCharactersId =
    let
        maybeCharacter =
            idToCharacter model.session.characters (Just selectedCharactersId)

        char =
            Maybe.withDefault emptyCharacter maybeCharacter
    in
    span [ class "tag is-small is-success" ] [ text char.name ]


buttonText : EditingState -> String
buttonText editing =
    case editing of
        NotEditing ->
            "Create Character"

        Editing _ ->
            "Update Character"
