module PlayerStatistics exposing (..)

import Html exposing (..)
import Romaji
import RomajiDict
import Scoring


placeHolder : Scoring.RomajiScoringDictionary -> List (Html msg)
placeHolder scores =
    let
        scoresAsList =
            RomajiDict.map
                (\romaji scoreInfo -> { romaji = Maybe.withDefault Romaji.A (Romaji.stringToRomaji romaji), score = scoreInfo.score })
                scores
                |> RomajiDict.values

        allScores =
            scoresAsList
                |> List.sortBy (\r -> Romaji.romajiToOrder r.romaji)
                |> List.map (\scoreInfo -> li [] [ text (Romaji.romajiToEnglish scoreInfo.romaji ++ " " ++ String.fromInt scoreInfo.score ++ ", ") ])
    in
    allScores
