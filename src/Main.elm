port module Main exposing
    ( Character
    , Model
    , Msg(..)
    , emptyModel
    , infoFooter
    , init
    , main
    , newCharacter
    , setStorage
    , update
    , updateWithStorage
    , view
    , viewCharacter
    , viewCharacters
    , viewForm
    , viewKeyedCharacter
    )

import Browser
import Browser.Dom as Dom
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json exposing (andThen, field)
import Json.Encode as E
import Task


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
    , form : Form
    , characters : List Character
    , uid : Int
    , editing : EditingState
    }


type alias Character =
    { name : String
    , hit_points : Int
    , armour : Int
    , initiative : Int
    , id : Int
    }


type alias Encounter =
    { characters : List Int
    , name : String
    , current : Int
    }


type alias Form =
    { name : String
    , hit_points : Int
    , armour : Int
    , initiative : Int
    }


emptyModel : Model
emptyModel =
    { characters = []
    , encounters = []
    , form =
        { name = ""
        , hit_points = 0
        , armour = 0
        , initiative = 0
        }
    , uid = 1
    , editing = NotEditing
    }


emptyCharacter =
    { name = ""
    , id = 0
    , armour = 0
    , hit_points = 0
    , initiative = 0
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
    List.map save model.characters


newCharacter : Form -> Int -> Character
newCharacter form id =
    { name = form.name
    , id = id
    , armour = form.armour
    , hit_points = form.hit_points
    , initiative = form.initiative
    }


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )


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
                , form =
                    { name = ""
                    , hit_points = 0
                    , armour = 0
                    , initiative = 0
                    }
                , editing = NotEditing
                , characters =
                    if String.isEmpty model.form.name then
                        model.characters

                    else
                        case model.editing of
                            NotEditing ->
                                model.characters ++ [ newCharacter model.form model.uid ]

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
                , form =
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
        , infoFooter
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
            [ text <| buttonText model.editing ]
        ]


buttonText editing =
    case editing of
        NotEditing ->
            "Create Character"

        Editing a ->
            "Update Character"



-- VIEW ALL characters


viewCharacters : List Character -> Html Msg
viewCharacters characters =
    let
        cssVisibility =
            if List.isEmpty characters then
                "hidden"

            else
                "visible"
    in
    section
        [ class "main"
        , style "visibility" cssVisibility
        ]
        [ h1 [] [ text "Characters" ]
        , Keyed.node "table" [ class "table is-bordered" ] <|
            List.map viewKeyedCharacter characters
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


infoFooter : Html msg
infoFooter =
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
        , ( "form", formToValue model.form )
        , ( "uid", E.int model.uid )
        , ( "editing", editingStateToValue model.editing )
        ]


characterToValue character =
    E.object
        [ ( "name", E.string character.name )
        , ( "hit_points", E.int character.hit_points )
        , ( "armour", E.int character.armour )
        , ( "initiative", E.int character.initiative )
        , ( "id", E.int character.id )
        ]


encounterToValue encounter =
    E.object
        [ ( "characters", E.list E.int encounter.characters )
        , ( "name", E.string encounter.name )
        , ( "current", E.int encounter.current )
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
        (field "form" formDecoder)
        (field "characters" (Json.list characterDecoder))
        (field "uid" Json.int)
        (field "editing" editingStateDecoder)


formDecoder : Json.Decoder Form
formDecoder =
    Json.map4 Form
        (field "name" Json.string)
        (field "armour" Json.int)
        (field "hit_points" Json.int)
        (field "initiative" Json.int)


characterDecoder : Json.Decoder Character
characterDecoder =
    Json.map5 Character
        (field "name" Json.string)
        (field "armour" Json.int)
        (field "hit_points" Json.int)
        (field "initiative" Json.int)
        (field "id" Json.int)


encounterDecoder : Json.Decoder Encounter
encounterDecoder =
    Json.map3 Encounter
        (field "characters" (Json.list Json.int))
        (field "name" Json.string)
        (field "current" Json.int)



-- editingStateDecoder : Json.Decoder EditingState


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

        Result.Err error ->
            Debug.log (Json.errorToString error) Nothing
