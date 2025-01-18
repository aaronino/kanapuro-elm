module Identify exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import ListShuffle
import Random
import Romaji
import RomajiDict
import Scoring
import String
import Task
import Time


numberOfTiles : number
numberOfTiles =
    6


type Msg
    = NextItem
    | ItemAnswered Romaji.Romaji
    | UpdatingScore Romaji.Romaji Time.Posix


type State
    = DisplayingItem Item
    | DisplayingAnswerResult Romaji.Romaji


type alias Model =
    { state : State
    , seed : Random.Seed
    , currentTarget : Romaji.Romaji
    , scoreInfo : Scoring.RomajiScoringDictionary
    }


type alias Item =
    { target : Romaji.Romaji
    , choices : List Romaji.Romaji
    }


init : Scoring.RomajiScoringDictionary -> ( Model, Cmd Msg )
init scoreInfo =
    let
        seed =
            Random.initialSeed 312

        ( nextItem, nextSeed ) =
            getNextItem seed scoreInfo

        -- todo get a random starting seed
    in
    ( Model (DisplayingItem nextItem) nextSeed nextItem.target scoreInfo
    , Cmd.none
    )


getNextItem : Random.Seed -> Scoring.RomajiScoringDictionary -> ( Item, Random.Seed )
getNextItem seed scoreInfo =
    getRandomItem seed scoreInfo (numberOfTiles - 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ItemAnswered answer ->
            let
                updateScoreForAnswer =
                    UpdatingScore answer
            in
            ( model
            , Task.perform updateScoreForAnswer Time.now
            )

        UpdatingScore answer answerTime ->
            let
                pointChange =
                    if answer == model.currentTarget then
                        2

                    else
                        -1

                updatedScore =
                    Scoring.updateScoreInfo model.scoreInfo pointChange model.currentTarget answerTime
            in
            ( { model | state = DisplayingAnswerResult answer, scoreInfo = updatedScore }
            , Cmd.none
            )

        NextItem ->
            let
                ( nextItem, nextSeed ) =
                    getNextItem model.seed model.scoreInfo
            in
            ( { model | state = DisplayingItem nextItem, currentTarget = nextItem.target, seed = nextSeed }
            , Cmd.none
            )


view : Model -> List (Html Msg)
view model =
    [ div [ class "text-2xl mt-5 mb-4 mx-10 leading-loose p-3 ring-1 ring-zinc-800 bg-sky-600 shadow-lg shadow-black rounded-sm" ]
        [ p [ class "text-center text-3xl " ] [ text "Identify" ]
        , div [] [ text "Find the matching character" ]
        ]
    , div [ class "mb-10 text-left mx-10" ]
        [ a [ href "home", class "text-red-600 text-lg transition-all ease-in-out hover:tracking-wide hover:text-red-600 duration-300 hover:text-xl" ] [ text "🠈 Back to games" ]
        ]
    , div [ class "text-center" ]
        [ case model.state of
            DisplayingAnswerResult answer ->
                answerDisplay model.currentTarget answer model.scoreInfo

            DisplayingItem currentItem ->
                itemDisplay currentItem
        ]
    ]


getRandomItem : Random.Seed -> Scoring.RomajiScoringDictionary -> Int -> ( Item, Random.Seed )
getRandomItem seed scoringInfo choiceCount =
    let
        ( target, _ ) =
            Scoring.findGoodTarget seed scoringInfo

        choicesWithoutTarget =
            List.filter (\r -> r /= target) Romaji.romajiAsList

        ( shuffledChoices, newSeed ) =
            ListShuffle.shuffleList seed choicesWithoutTarget

        filteredChoices =
            List.take choiceCount shuffledChoices

        ( shuffledChoicesWithTarget, _ ) =
            ListShuffle.shuffleList newSeed (target :: filteredChoices)
    in
    -- Get a random Hiragana item - a set of a question and choiceCount answers
    -- The "real" answer will be appended to the list of random choices to give us an extra (correct) choice
    ( { target = target, choices = shuffledChoicesWithTarget }, newSeed )


itemDisplay : Item -> Html Msg
itemDisplay item =
    let
        target =
            Romaji.romajiToHiragana item.target
    in
    div []
        [ div [ class "flex justify-center content-center" ]
            [ p [ class "text-5xl text-white bg-zinc-950 rounded-lg h-36 w-36 content-center ring ring-slate-600" ]
                [ text target ]
            ]
        , choiceGrid item.choices
        ]


choiceGrid : List Romaji.Romaji -> Html Msg
choiceGrid choices =
    div
        [ class "grid gap-10 grid-cols-3 mt-10 mb-10" ]
        (List.map choiceBox choices)


choiceBox : Romaji.Romaji -> Html Msg
choiceBox choice =
    let
        choiceText =
            Romaji.romajiToEnglish choice
    in
    div []
        [ button
            [ onClick (ItemAnswered choice)
            , class "text-5xl text-white bg-zinc-950 rounded-lg h-36 w-36 content-center ring ring-yellow-600 hover:ring-sky-600 hover:from-sky-600 hover:to-zinc-950 bg-gradient-to-bl"
            ]
            [ text choiceText ]
        ]


answerDisplay : Romaji.Romaji -> Romaji.Romaji -> Scoring.RomajiScoringDictionary -> Html Msg
answerDisplay target answer scores =
    let
        isCorrect =
            target == answer

        correctAnswerDisplay =
            h2 [ style "color" "green" ] [ text "Correct!" ]

        wrongAnswerDisplay =
            h2 [ style "color" "orange" ] [ text "Incorrect" ]

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
    div []
        [ h1 [] [ text ("Question: " ++ Romaji.romajiToHiragana target) ]
        , h1 [] [ text ("Correct Answer: " ++ Romaji.romajiToEnglish target) ]
        , h1 [] [ text ("Your Answer: " ++ Romaji.romajiToEnglish answer) ]
        , if isCorrect then
            correctAnswerDisplay

          else
            wrongAnswerDisplay
        , button
            [ onClick NextItem
            , style "padding" "100px"
            , style "margin" "20px"
            , style "font-size" "40px"
            ]
            [ text "Next Question" ]
        ]
