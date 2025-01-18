module Memory exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Identify exposing (State(..))
import List
import MemoryQuiz
import Random
import Romaji


type Msg
    = FetchItem
    | ItemReceived MemoryQuiz.Item
    | TargetChosen Romaji.Romaji


type State
    = FetchingItem
    | DisplayingItem MemoryQuiz.Item
    | DisplayingAnswerResult { targets : List Romaji.Romaji, choices : List Romaji.Romaji }


type alias Model =
    { state : State
    , seed : Random.Seed
    , targets : List Romaji.Romaji
    , choices : List Romaji.Romaji
    , choiceCount : Int
    , targetCount : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        seed =
            Random.initialSeed 312

        startingTargetCount =
            3

        startingChoiceCount =
            9

        -- todo get a random starting seed and update the model seed every time we use it
    in
    ( Model FetchingItem seed [] [] startingChoiceCount startingTargetCount
    , getNextItem seed startingTargetCount startingChoiceCount
    )


getNextItem : Random.Seed -> Int -> Int -> Cmd Msg
getNextItem seed choiceCount targetCount =
    Random.generate ItemReceived (MemoryQuiz.getRandomItem seed choiceCount targetCount)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TargetChosen choice ->
            let
                newState =
                    if model.choiceCount == List.length model.choices then
                        DisplayingAnswerResult { targets = model.targets, choices = model.choices }

                    else
                        model.state
            in
            ( { model | state = newState, choices = choice :: model.choices, choiceCount = model.choiceCount + 1 }
            , Cmd.none
            )

        ItemReceived item ->
            ( { model | state = DisplayingItem item, targets = item.targets }
            , Cmd.none
            )

        FetchItem ->
            ( { model | state = FetchingItem }
            , getNextItem model.seed model.choiceCount model.targetCount
            )


view : (Msg -> msg) -> Model -> List (Html msg)
view toMainMsg model =
    [ h2 [] [ text "Memory" ]
    , div [ style "text-align" "center" ]
        [ case model.state of
            DisplayingAnswerResult { choices, targets } ->
                answerDisplay toMainMsg choices targets

            DisplayingItem currentItem ->
                itemDisplay toMainMsg currentItem

            FetchingItem ->
                div [] []
        ]
    ]


itemDisplay : (Msg -> msg) -> MemoryQuiz.Item -> Html msg
itemDisplay toMainMsg item =
    let
        targets =
            List.map Romaji.romajiToHiragana item.targets

        questionDisplay =
            List.map (\target -> li [ style "font-size" "50px", style "display" "inline" ] [ text target ]) targets
    in
    div []
        [ div [] [ text "Remember these characters..." ]
        , ul [] questionDisplay
        , choiceGrid toMainMsg item.choices
        ]


choiceGrid : (Msg -> msg) -> List Romaji.Romaji -> Html msg
choiceGrid toMainMsg choices =
    let
        choiceBoxWithToMainMsg =
            choiceBox toMainMsg
    in
    div
        [ style "display" "grid"
        , style "gap" "1px"
        , style "grid-template-columns" "repeat(3, 1fr)"
        ]
        (List.map choiceBoxWithToMainMsg choices)


choiceBox : (Msg -> msg) -> Romaji.Romaji -> Html msg
choiceBox toMainMsg choice =
    let
        choiceText =
            Romaji.romajiToEnglish choice
    in
    div []
        [ button
            [ onClick (toMainMsg (TargetChosen choice))
            , style "padding" "50px"
            , style "margin" "10px"
            , style "font-size" "20px"
            ]
            [ text choiceText ]
        ]


answerDisplay : (Msg -> msg) -> List Romaji.Romaji -> List Romaji.Romaji -> Html msg
answerDisplay toMainMsg target answer =
    let
        isCorrect =
            target == answer

        correctAnswerDisplay =
            h2 [ style "color" "green" ] [ text "Correct!" ]

        wrongAnswerDisplay =
            h2 [ style "color" "orange" ] [ text "Incorrect" ]
    in
    div []
        [ h1 [] [ text "todo - display answers" ]
        , if isCorrect then
            correctAnswerDisplay

          else
            wrongAnswerDisplay
        , button
            [ onClick (toMainMsg FetchItem)
            , style "padding" "100px"
            , style "margin" "20px"
            , style "font-size" "40px"
            ]
            [ text "Next Question" ]
        ]
