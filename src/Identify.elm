module Identify exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import ListShuffle
import Random
import Romaji
import Scoring
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
    | DisplayingAnswerResult Item Romaji.Romaji


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

                currentItem =
                    case model.state of
                        DisplayingItem item ->
                            item

                        _ ->
                            -- impossible state, log error
                            Item Romaji.A []

                updatedScore =
                    Scoring.updateScoreInfo model.scoreInfo pointChange model.currentTarget answerTime
            in
            ( { model | state = DisplayingAnswerResult currentItem answer, scoreInfo = updatedScore }
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
    , div [ class "grid grid-cols-5 mb-10 mx-10" ]
        [ div [ class "col-span-4 text-left pr-4" ]
            [ div [ class "flex justify-between mb-1 flex-inline h-full items-center" ]
                [ div [ class "w-full bg-gray-200 rounded-full h-6 dark:bg-gray-700" ]
                    [ div [ class "bg-amber-600 h-6 rounded-full", style "width" "50%" ] [] ]
                ]
            ]
        , a
            [ href "home"
            , class "text-white bg-slate-600 text-lg p-1 rounded-md transition-all ease-in-out hover:tracking-wide duration-300"
            ]
            [ text "🠈 Back to games" ]
        ]
    , div [ class "text-center" ]
        [ case model.state of
            DisplayingAnswerResult currentItem answer ->
                answerDisplay currentItem answer

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
            [ p [ class "text-5xl text-white bg-zinc-950 rounded-lg h-36 w-36 content-center ring ring-white" ]
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
            , class
                ("content-center text-5xl h-36 w-36 text-white from-zinc-950 to-zinc-900 bg-gradient-to-tr ring ring-yellow-600 "
                    ++ "transition-all hover:ring-sky-600 hover:text-sky-300 hover:to-zinc-950 duration-300"
                )
            ]
            [ text choiceText ]
        ]


answerDisplay : Item -> Romaji.Romaji -> Html Msg
answerDisplay item answer =
    let
        isCorrect =
            item.target == answer

        answerClasses =
            if isCorrect then
                "text-5xl text-green-600 bg-zinc-950 rounded-lg h-36 w-36 content-center ring ring-green-600"

            else
                "text-5xl text-white bg-zinc-950 rounded-lg h-36 w-36 content-center ring ring-white"
    in
    div [ onClick NextItem, class "cursor-pointer" ]
        [ div [ class "grid gap-10 grid-cols-3" ]
            [ div [] []
            , div [ class "flex justify-center" ]
                [ p [ class answerClasses ]
                    [ text (Romaji.romajiToHiragana item.target)
                    , br [] []
                    , text (Romaji.romajiToEnglish item.target)
                    ]
                ]
            , div [ class "flex justify-center" ] []
            ]
        , answerGrid item.choices item.target answer
        , div [ class "flex flex-row justify-center content-center w-full" ] []
        , div []
            [ button
                [ onClick NextItem
                , class "content-center h-12 w-36 text-xl text-slate-50 bg-gradient-to-tr from-green-600 to-green-800 shadow-lg shadow-blue ring-1 ring-slate-600 rounded-md"
                ]
                [ text "Next ➔" ]
            ]
        ]


getAnswerState : Romaji.Romaji -> Romaji.Romaji -> Romaji.Romaji -> AnswerState
getAnswerState romajiToDisplay targetRomaji answeredRomaji =
    if romajiToDisplay == targetRomaji then
        Correct

    else if romajiToDisplay == answeredRomaji then
        Incorrect

    else
        Inert


answerGrid : List Romaji.Romaji -> Romaji.Romaji -> Romaji.Romaji -> Html Msg
answerGrid choices target answer =
    let
        answerBoxes =
            List.map
                (\choice ->
                    let
                        answerState =
                            getAnswerState choice target answer
                    in
                    answerBox choice answerState
                )
                choices
    in
    div
        [ class "grid gap-10 grid-cols-3 mt-10 mb-10" ]
        answerBoxes


type AnswerState
    = Inert
    | Correct
    | Incorrect


answerBox : Romaji.Romaji -> AnswerState -> Html Msg
answerBox choice answerState =
    let
        choiceTextEnglish =
            Romaji.romajiToEnglish choice

        choiceTextHiragana =
            Romaji.romajiToHiragana choice

        classes =
            case answerState of
                Inert ->
                    "content-center text-5xl h-36 w-36 text-white from-zinc-950 to-zinc-900 bg-gradient-to-tr ring ring-yellow-600 "

                Correct ->
                    "content-center text-5xl h-36 w-36 text-green-600 from-zinc-950 to-zinc-900 bg-gradient-to-tr ring ring-green-600 "

                Incorrect ->
                    "content-center text-5xl h-36 w-36 text-red-600 from-zinc-950 to-zinc-900 bg-gradient-to-tr ring ring-red-600 "
    in
    div []
        [ button
            [ onClick NextItem, class classes ]
            [ text choiceTextHiragana
            , br [] []
            , text choiceTextEnglish
            ]
        ]
