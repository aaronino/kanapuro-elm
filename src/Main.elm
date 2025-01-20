module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Heroicons.Outline
import Html exposing (..)
import Html.Attributes exposing (..)
import Identify
import Memory
import Scoring
import Svg.Attributes
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
    | Speed


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | IdentifyMsg Identify.Msg
    | MemoryMsg Memory.Msg



-- MODEL


type alias Model =
    { key : Nav.Key
    , currentRoute : Route
    , identifyModel : Identify.Model
    , memoryModel : Memory.Model
    , romajiScoreInfo : Scoring.RomajiScoringDictionary
    }


type alias FullRouteData =
    { href : String
    , text : String
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
        , Parser.map Settings (Parser.s "settings")
        , Parser.map Account (Parser.s "account")
        , Parser.map About (Parser.s "about")
        , Parser.map ScoreData (Parser.s "score_data")
        , Parser.map Speed (Parser.s "speed")
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
                    staticPageLayout (Memory.view MemoryMsg model.memoryModel)

                _ ->
                    staticPageLayout frontPageDisplay
    in
    { title = "Kanapuro"
    , body =
        currentDisplay
    }


staticPageLayout : List (Html Msg) -> List (Html Msg)
staticPageLayout innerContent =
    [ div [ class "flex justify-center w-full h-full text-sky-50 bg-zinc-950 " ]
        [ div [] []
        , div [ class "w-10/12 max-w-4xl" ]
            -- top bar
            [ div [ class "flex justify-between items-end h-20 p-5 mt-2 " ]
                [ div [ class "text-4xl text-sky-600 tracking-tighter transition-all duration-300 hover:tracking-wide hover:text-sky-300 " ]
                    [ a [ href (routeToFullRouteData Home).href ] [ text "kanapuro" ]
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
            , div [ class "justify-center text-center p-3 mx-2 my-3 bg-zinc-900 ring-2 ring-zinc-800 rounded-lg" ]
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
                [ Identify
                , Memory
                , Typing
                , Speed
                ]
    in
    [ div [ class "my-10 mx-10 p-3 text-2xl leading-loose bg-slate-600 ring-1 ring-zinc-800 shadow-lg shadow-black" ]
        [ text
            ("Kanapuro is a game for learning Hiragana quickly and efficiently.\nChoose\n from a variety of exercises "
                ++ "to practice, or let Kanapuro lead you through exercises to master Hiragana."
            )
        ]
    , div [ class "flex flex-row flex-wrap justify-center mt-12" ]
        [ div [ class "flex flex-row justify-center content-center h-48 w-2/3" ]
            [ a
                [ href "/identify"
                , class
                    ("content-center text-center w-full h-auto m-6 text-slate-50 from-zinc-950 to-zinc-900 bg-gradient-to-tr shadow-lg shadow-blue ring ring-amber-600 rounded-md"
                        ++ "transition-all duration-300 hover:text-sky-600 hover:ring-sky-600"
                    )
                ]
                [ div [ class "flex flex-col justify-center text-5xl" ]
                    [ div
                        [ class "m-auto" ]
                        [ Heroicons.Outline.academicCap [ Svg.Attributes.width "3rem", Svg.Attributes.height "3rem" ]
                        ]
                    , p [ class "text-lg" ] [ text "Quick start" ]
                    ]
                ]
            ]
        ]
    , p [ class "text-3xl" ] [ text "or choose your game..." ]
    , div [ class "flex flex-row flex-wrap justify-start mt-6" ] buttons
    , div [ class "m-4" ]
        [ a
            [ href (routeToFullRouteData ScoreData).href
            , class "p-4 text-white text-lg bg-slate-600 transition-all hover:tracking-wide duration-300 rounded-md"
            ]
            [ text "See my stats" ]
        ]
    ]


createGameButton : Route -> Html Msg
createGameButton route =
    let
        routeData =
            routeToFullRouteData route

        icon =
            getRouteIcon route
    in
    div [ class "flex flex-row justify-center content-center h-48 w-1/3 text-5xl" ]
        [ a
            [ href routeData.href
            , class
                ("content-center text-center w-48 h-auto m-6 text-slate-50 from-zinc-950 to-zinc-900 bg-gradient-to-tr shadow-lg shadow-blue ring ring-amber-600 rounded-md"
                    ++ "transition-all duration-300 hover:text-sky-600 hover:ring-sky-600"
                )
            ]
            [ div [ class "flex flex-col justify-center" ]
                [ div
                    [ class "m-auto" ]
                    [ icon [ Svg.Attributes.width "3rem", Svg.Attributes.height "3rem" ] ]
                , p [ class "text-lg" ] [ text routeData.text ]
                ]
            ]
        ]


routeToFullRouteData : Route -> FullRouteData
routeToFullRouteData route =
    case route of
        Home ->
            { href = "/home", text = "Home" }

        Identify ->
            { href = "/identify", text = "Identify" }

        Memory ->
            { href = "/memory", text = "Memory" }

        Typing ->
            { href = "/typing", text = "Typing" }

        Speed ->
            { href = "/speed", text = "Speed" }

        _ ->
            { href = "/construction", text = "construction" }


getRouteIcon : Route -> (List (Attribute Msg) -> Html Msg)
getRouteIcon route =
    case route of
        Identify ->
            Heroicons.Outline.tableCells

        Memory ->
            Heroicons.Outline.adjustmentsHorizontal

        Typing ->
            Heroicons.Outline.chatBubbleLeftEllipsis

        Speed ->
            Heroicons.Outline.wifi

        _ ->
            Heroicons.Outline.playCircle



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
