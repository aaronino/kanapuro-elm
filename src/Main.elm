module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Identify
import Memory
import Scoring
import Url
import Url.Parser as Parser


type Route
    = Home
    | Identify
    | Memory
    | Typing
    | Settings
    | Account
    | About
    | ScoreData



-- MODEL


type alias Model =
    { key : Nav.Key
    , currentRoute : Route
    , identifyModel : Identify.Model
    , memoryModel : Memory.Model
    , romajiScoreInfo : Scoring.RomajiScoringDictionary
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    let
        -- Initialize Identify model and its commands
        ( identifyInitialModel, identifyInitialCmd ) =
            Identify.init Scoring.romajiScoringDict

        ( memoryInitialModel, memoryInitialCmd ) =
            Memory.init
    in
    ( { key = key
      , currentRoute = Home
      , identifyModel = identifyInitialModel
      , memoryModel = memoryInitialModel
      , romajiScoreInfo = Scoring.romajiScoringDict
      }
    , Cmd.batch
        [ Cmd.map IdentifyMsg identifyInitialCmd
        , Cmd.map MemoryMsg memoryInitialCmd
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | IdentifyMsg Identify.Msg
    | MemoryMsg Memory.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                parsedRoute =
                    Maybe.withDefault Home (Parser.parse routeParser url)
            in
            ( { model | currentRoute = parsedRoute }
            , Cmd.none
            )

        IdentifyMsg identifyMsg ->
            let
                ( updatedIdentifyModel, identifyCmd ) =
                    Identify.update identifyMsg model.identifyModel
            in
            ( { model | identifyModel = updatedIdentifyModel }, Cmd.map IdentifyMsg identifyCmd )

        MemoryMsg memoryMsg ->
            let
                ( updatedMemoryModel, memoryCmd ) =
                    Memory.update memoryMsg model.memoryModel
            in
            ( { model | memoryModel = updatedMemoryModel }, Cmd.map MemoryMsg memoryCmd )


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home (Parser.s "home")
        , Parser.map Identify (Parser.s "identify")
        , Parser.map Memory (Parser.s "memory")
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        currentDisplay =
            case model.currentRoute of
                Identify ->
                    let
                        convertIdentifyMsgToHomeMsg =
                            Html.map IdentifyMsg

                        identifyViewBase =
                            Identify.view model.identifyModel

                        identifyView =
                            List.map convertIdentifyMsgToHomeMsg identifyViewBase
                    in
                    staticPageLayout identifyView

                Memory ->
                    miniNavigationDisplay [ Home, Identify ]
                        ++ header
                        ++ Memory.view MemoryMsg model.memoryModel

                _ ->
                    staticPageLayout frontPageDisplay
    in
    { title = "Kanapuro"
    , body =
        currentDisplay
    }


staticPageLayout : List (Html Msg) -> List (Html Msg)
staticPageLayout innerContent =
    [ div [ class "flex justify-center w-full h-full min-h-full bg-zinc-950 text-sky-50" ]
        [ div [] []
        , div [ class "w-10/12 max-w-4xl" ]
            -- top bar
            [ div [ class "flex justify-between h-20 p-5 mt-2 items-end" ]
                [ div [ class "text-4xl text-sky-600 transition-all ease-in-out tracking-tighter hover:tracking-wide hover:text-sky-300 duration-300" ]
                    [ a [ href (routeToLinkUrlAndDisplay Home).href ] [ text "kanapuro" ]
                    ]
                , div []
                    [ ul [ class "inline-flex space-x-8 text-sm" ]
                        [ li [] [ text "Settings" ]
                        , li [] [ text "About" ]
                        , li [] [ text "Account" ]
                        ]
                    ]
                ]

            -- primary content
            , div [ class "rounded-lg p-3 bg-zinc-900 mx-2 my-3 justify-center text-center ring-2 ring-zinc-800" ]
                innerContent
            ]
        , div [] []
        ]
    ]


frontPageDisplay : List (Html Msg)
frontPageDisplay =
    let
        buttons =
            List.map createGameButton
                [ routeToLinkUrlAndDisplay Identify
                , routeToLinkUrlAndDisplay Memory
                , routeToLinkUrlAndDisplay Typing
                , routeToLinkUrlAndDisplay Identify
                , routeToLinkUrlAndDisplay Identify
                ]
    in
    [ div [ class "text-2xl my-10 mx-10 leading-loose p-3 ring-1 ring-zinc-800 bg-slate-600 shadow-lg shadow-black" ]
        [ text
            ("Kanapuro is a game for learning Hiragana quickly and efficiently.\nChoose\n from a variety of exercises "
                ++ "to practice, or let Kanapuro lead you through exercises to master Hiragana."
            )
        ]
    , p [ class "mt-12 text-3xl" ] [ text "Choose your game..." ]
    , div [ class "mt-6 flex flex-row flex-wrap justify-start" ] buttons
    ]


createGameButton : LinkUrlAndDisplay -> Html Msg
createGameButton game =
    div [ class "text-2xl h-48 justify-center content-center w-1/3 flex flex-row" ]
        [ a [ href game.href, class "w-48 h-auto m-6 text-slate-50 from-sky-600 to-sky-800 shadow-lg shadow-blue content-center ring-1 ring-slate-600 rounded-md bg-gradient-to-tr" ]
            [ text game.text ]
        ]


header : List (Html Msg)
header =
    [ h1 [ class "text-3xl p-20" ] [ text "Kanapuro" ] ]


miniNavigationDisplay : List Route -> List (Html Msg)
miniNavigationDisplay games =
    [ miniNavigationGameGrid games
    ]


miniNavigationGameGrid : List Route -> Html Msg
miniNavigationGameGrid games =
    div
        [ style "display" "flex"
        , style "gap" "1px"
        , style "justify-content" "flex-start"
        ]
        (List.map miniNavigationGameCell games)


miniNavigationGameCell : Route -> Html Msg
miniNavigationGameCell game =
    let
        link =
            routeToLinkUrlAndDisplay game
    in
    div []
        [ a
            [ href link.href
            ]
            [ button
                [ style "padding" "10px"
                , style "margin" "5px"
                , style "font-size" "15px"
                ]
                [ text link.text ]
            ]
        ]


type alias LinkUrlAndDisplay =
    { href : String, text : String }


routeToLinkUrlAndDisplay : Route -> LinkUrlAndDisplay
routeToLinkUrlAndDisplay route =
    case route of
        Home ->
            LinkUrlAndDisplay "/home" "Home"

        Identify ->
            LinkUrlAndDisplay "/identify" "Identify"

        Memory ->
            LinkUrlAndDisplay "/memory" "Memory"

        Typing ->
            LinkUrlAndDisplay "/typing" "Typing"

        _ ->
            LinkUrlAndDisplay "/construction" "construction"



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
