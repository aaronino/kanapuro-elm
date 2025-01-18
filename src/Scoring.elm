module Scoring exposing (..)

import Dict exposing (Dict)
import ListShuffle
import Random
import Romaji
import RomajiDict
import Time


type alias ScoreData =
    { score : Int
    , lastAsked : Time.Posix
    }


type alias RomajiScoringDictionary =
    RomajiDict.RomajiDict ScoreData


findBestTarget : Dict String ScoreData -> Romaji.Romaji
findBestTarget scores =
    let
        compareScore : String -> ScoreData -> ( Romaji.Romaji, ScoreData ) -> ( Romaji.Romaji, ScoreData )
        compareScore romaji scoreInfo previous =
            let
                previousScore =
                    Tuple.second previous

                previousAsked =
                    Time.posixToMillis previousScore.lastAsked

                newAsked =
                    Time.posixToMillis scoreInfo.lastAsked

                newValue =
                    ( Maybe.withDefault Romaji.A (Romaji.stringToRomaji romaji), scoreInfo )
            in
            if scoreInfo.score < previousScore.score then
                newValue

            else if scoreInfo.score == previousScore.score && newAsked < previousAsked then
                newValue

            else
                previous
    in
    -- go through every element of the Dict and find the first one with the lowest score with the lowest lastAsked score
    Dict.foldl compareScore ( Romaji.A, ScoreData 10000 (Time.millisToPosix 0) ) scores
        |> Tuple.first


type alias RomajiAndScoreInfo =
    { romaji : Romaji.Romaji
    , score : Int
    , lastAsked : Time.Posix
    }


romajiScoringDict : RomajiScoringDictionary
romajiScoringDict =
    RomajiDict.Romaji (Dict.fromList (List.map (\r -> ( r, { score = 0, lastAsked = Time.millisToPosix 0 } )) Romaji.englishToList))


toRomajiAndScoreInfo : ( String, ScoreData ) -> RomajiAndScoreInfo
toRomajiAndScoreInfo ( romajiString, score ) =
    let
        romaji =
            Maybe.withDefault Romaji.A (Romaji.stringToRomaji romajiString)
    in
    { romaji = romaji, score = score.score, lastAsked = score.lastAsked }


findGoodTarget : Random.Seed -> RomajiScoringDictionary -> ( Romaji.Romaji, Random.Seed )
findGoodTarget seed scores =
    -- differs from findbesttarget because here we choose a random target from poolSize possible targets
    let
        randomScorePoolSize =
            5

        -- how many romaji do we consider when getting our target?
        ( filteredScores, newSeed ) =
            RomajiDict.toList scores
                |> List.map toRomajiAndScoreInfo
                |> List.sortWith sortRomajiAndScoreInfoByOrder
                |> List.sortWith compareRomajiAndScoreInfo
                |> List.take randomScorePoolSize
                |> ListShuffle.shuffleList seed

        defaultRomajiWithScore =
            { romaji = Romaji.A, score = 0, lastAsked = Time.millisToPosix 0 }

        singleScoreItem =
            Maybe.withDefault defaultRomajiWithScore (List.head filteredScores)
                |> .romaji
    in
    ( singleScoreItem, newSeed )


sortRomajiAndScoreInfoByOrder : RomajiAndScoreInfo -> RomajiAndScoreInfo -> Order
sortRomajiAndScoreInfoByOrder a b =
    compare (Romaji.romajiToOrder a.romaji) (Romaji.romajiToOrder b.romaji)


compareRomajiAndScoreInfo : RomajiAndScoreInfo -> RomajiAndScoreInfo -> Order
compareRomajiAndScoreInfo a b =
    case compare a.score b.score of
        EQ ->
            compare (Time.posixToMillis a.lastAsked) (Time.posixToMillis b.lastAsked)

        other ->
            other


updateScoreInfo : RomajiScoringDictionary -> Int -> Romaji.Romaji -> Time.Posix -> RomajiScoringDictionary
updateScoreInfo scores increment romaji time =
    let
        tryUpdateScore : Maybe ScoreData -> Maybe ScoreData
        tryUpdateScore score =
            Maybe.map updateScore score

        updateScore : ScoreData -> ScoreData
        updateScore scoreInfo =
            { scoreInfo | score = max -10 (scoreInfo.score + increment), lastAsked = time }
    in
    -- update the score for dictionary character romaji by increment
    RomajiDict.update romaji tryUpdateScore scores
