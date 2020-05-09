module Session exposing (Data, addCharacters, emptyData)

import Character.Types exposing (Character)
import Encounter exposing (Encounter)


type alias Data =
    { encounters : List Encounter
    , characters : List Character
    , uid : Int
    }


emptyData : Data
emptyData =
    { characters = []
    , encounters = []
    , uid = 1
    }


addCharacters : List Character -> Data -> Data
addCharacters characters data =
    { data | characters = characters }
