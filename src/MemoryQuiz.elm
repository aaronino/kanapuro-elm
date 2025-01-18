module MemoryQuiz exposing (..)

import ListShuffle
import Random
import Romaji


type alias Item =
    { targets : List Romaji.Romaji
    , choices : List Romaji.Romaji
    }


getRandomItem : Random.Seed -> Int -> Int -> Random.Generator Item
getRandomItem seed targetCount choiceCount =
    -- Get a random Hiragana item - a set of a question and choiceCount answers
    -- We get choiceCount - 1 fake answers because the "real" answer will be appended to the list of choices
    Random.map2 (createItem seed) (Romaji.getRandomRomajiList targetCount) (Romaji.getRandomRomajiList (choiceCount - 1))


createItem : Random.Seed -> List Romaji.Romaji -> List Romaji.Romaji -> Item
createItem seed targets choices =
    -- Helper function to construct the record and shuffle choices
    let
        shuffledTargets =
            ListShuffle.shuffleList seed choices |> (\( c, _ ) -> c)

        shuffledChoices =
            ListShuffle.shuffleList seed (targets ++ choices) |> (\( c, _ ) -> c)

        -- todo here we are throwing away our new seed after our first seed is used
    in
    { targets = shuffledTargets, choices = shuffledChoices }
