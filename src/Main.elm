port module Main exposing
    ( Model
    , Msg(..)
    , init
    , main
    , setStorage
    , update
    , updateWithStorage
    , view
    )

import Browser
import Character
    exposing
        ( Character
        , characterDecoder
        , characterToValue
        , emptyCharacter
        , emptyCharacterForm
        , newCharacter
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


main : Program (Maybe E.Value) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "D&D Encounter App", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : E.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage (modelToValue newModel), cmds ]
    )



-- MODEL


type EditingState
    = Editing Int
    | NotEditing


type alias Model =
    { encounters : List Encounter
    , characterForm : Character.Form
    , characters : List Character
    , uid : Int
    , editing : EditingState
    }


emptyModel : Model
emptyModel =
    { characters = []
    , encounters = []
    , characterForm = Character.newCharacterForm
    , uid = 1
    , editing = NotEditing
    }


saveCharacter : Model -> Int -> List Character
saveCharacter model id =
    let
        save character =
            if character.id == id then
                { name = model.characterForm.name
                , id = id
                , armour = model.characterForm.armour
                , hit_points = model.characterForm.hit_points
                , initiative = model.characterForm.initiative
                }

            else
                character
    in
    List.map save model.characters


updateForm : (Character.Form -> Character.Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | characterForm = transform model.characterForm }, Cmd.none )


init : Maybe E.Value -> ( Model, Cmd Msg )
init savedModel =
    let
        model =
            case savedModel of
                Just value ->
                    Maybe.withDefault emptyModel (Json.decodeValue modelDecoder value |> resultToMaybe)

                _ ->
                    emptyModel
    in
    ( model, Cmd.none )



-- UPDATE


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
            ( { model
                | uid = model.uid + 1
                , characterForm = emptyCharacterForm
                , editing = NotEditing
                , characters =
                    if String.isEmpty model.characterForm.name then
                        model.characters

                    else
                        case model.editing of
                            NotEditing ->
                                model.characters ++ [ newCharacter model.characterForm model.uid ]

                            Editing id ->
                                saveCharacter model id
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
                    List.head (List.filter (\c -> c.id == id) model.characters)

                character =
                    Maybe.withDefault emptyCharacter maybeCharacter
            in
            ( { model
                | uid = id
                , characterForm =
                    { name = character.name
                    , hit_points = character.hit_points
                    , armour = character.armour
                    , initiative = character.initiative
                    }
                , editing = Editing id
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
            in
            ( { model | characters = List.map updateCharacter model.characters }
            , Cmd.none
            )

        Delete id ->
            ( { model | characters = List.filter (\t -> t.id /= id) model.characters }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ section
            [ class "content" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-third" ] [ viewForm model ]
                , div [ class "column" ] [ lazy viewCharacters model.characters ]
                ]
            ]
        , viewFooter
        ]


viewForm : Model -> Html Msg
viewForm model =
    let
        form =
            model.characterForm
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
            [ text <| buttonText model.editing ]
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


modelToValue model =
    E.object
        [ ( "characters", E.list characterToValue model.characters )
        , ( "encounters", E.list encounterToValue model.encounters )
        , ( "characterForm", formToValue model.characterForm )
        , ( "uid", E.int model.uid )
        , ( "editing", editingStateToValue model.editing )
        ]


editingStateToValue editingState =
    case editingState of
        Editing a ->
            E.int a

        NotEditing ->
            E.null


formToValue form =
    E.object
        [ ( "name", E.string form.name )
        , ( "hit_points", E.int form.hit_points )
        , ( "armour", E.int form.armour )
        , ( "initiative", E.int form.initiative )
        ]


modelDecoder =
    Json.map5 Model
        (field "encounters" (Json.list encounterDecoder))
        (field "characterForm" Character.formDecoder)
        (field "characters" (Json.list characterDecoder))
        (field "uid" Json.int)
        (field "editing" editingStateDecoder)


editingStateDecoder =
    Json.oneOf [ Json.int, Json.null 0 ]
        |> andThen
            (\num ->
                case num of
                    0 ->
                        Json.succeed NotEditing

                    a ->
                        Json.succeed (Editing a)
            )


resultToMaybe result =
    case result of
        Result.Ok model ->
            Just model

        Result.Err _ ->
            -- Debug.log (Json.errorToString error) Nothing
            Nothing
