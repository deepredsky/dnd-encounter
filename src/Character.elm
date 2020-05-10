module Character exposing
    ( Form
    , Model
    , Msg
    , characterDecoder
    , characterToValue
    , emptyCharacter
    , emptyCharacterForm
    , formDecoder
    , init
    , newCharacter
    , newCharacterForm
    , update
    , view
    , viewForm
    )

import Browser
import Character.Types exposing (Character)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode exposing (andThen, field)
import Json.Encode as Encode
import Session exposing (Data)


type alias Form =
    { name : String
    , hit_points : Int
    , armour : Int
    , initiative : Int
    , editing : EditingState
    }


type alias Model =
    { session : Session.Data
    , form : Form
    }


type EditingState
    = Editing Int
    | NotEditing


newCharacterForm =
    { name = ""
    , hit_points = 0
    , armour = 0
    , initiative = 0
    , editing = NotEditing
    }


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( Model session newCharacterForm, Cmd.none )


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
    Decode.map5 Form
        (field "name" Decode.string)
        (field "armour" Decode.int)
        (field "hit_points" Decode.int)
        (field "initiative" Decode.int)
        (field "editing" editingStateDecoder)


editingStateDecoder =
    Decode.oneOf [ Decode.int, Decode.null 0 ]
        |> andThen
            (\num ->
                case num of
                    0 ->
                        Decode.succeed NotEditing

                    a ->
                        Decode.succeed (Editing a)
            )


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
    , editing = NotEditing
    }


saveCharacter : Model -> Int -> List Character
saveCharacter model id =
    let
        save character =
            if character.id == id then
                { name = model.form.name
                , id = id
                , armour = model.form.armour
                , hit_points = model.form.hit_points
                , initiative = model.form.initiative
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
            updateForm (\form -> { form | hit_points = newHitPoint }) model

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
                    , hit_points = character.hit_points
                    , armour = character.armour
                    , initiative = character.initiative
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


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- view : Model -> Browser.Document Msg


view model =
    div
        [ class "container" ]
        [ section
            [ class "content" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-third" ] [ viewForm model ]
                , div [ class "column" ] [ lazy viewCharacters model.session.characters ]
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
                , value (String.fromInt form.hit_points)
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
        [ td [] [ text character.name ]
        , td [] [ text (String.fromInt character.hit_points) ]
        , td [] [ text (String.fromInt character.armour) ]
        , td [] [ text (String.fromInt character.initiative) ]
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


buttonText editing =
    case editing of
        NotEditing ->
            "Create Character"

        Editing _ ->
            "Update Character"
