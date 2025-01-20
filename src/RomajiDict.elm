module RomajiDict exposing (RomajiDict(..), map, toList, update, values)

import Dict exposing (Dict)
import Romaji


type RomajiDict a
    = Romaji (Dict String a)


toList : RomajiDict a -> List ( String, a )
toList (Romaji dict) =
    Dict.toList dict


update : Romaji.Romaji -> (Maybe a -> Maybe a) -> RomajiDict a -> RomajiDict a
update romaji func (Romaji dict) =
    Dict.update (Romaji.romajiToEnglish romaji) func dict
        |> Romaji


map : (String -> a -> v) -> RomajiDict a -> RomajiDict v
map func (Romaji dict) =
    Dict.map func dict
        |> Romaji


values : RomajiDict a -> List a
values (Romaji dict) =
    Dict.values dict
