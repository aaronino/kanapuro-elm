module Romaji exposing (Romaji(..), englishToList, getRandomRomaji, getRandomRomajiList, romajiAsList, romajiToEnglish, romajiToHiragana, romajiToOrder, stringToRomaji)

import Random


type Romaji
    = A
    | I
    | U
    | E
    | O
    | Ka
    | Ki
    | Ku
    | Ke
    | Ko
    | Sa
    | Shi
    | Su
    | Se
    | So
    | Ta
    | Chi
    | Tsu
    | Te
    | To
    | Na
    | Ni
    | Nu
    | Ne
    | No
    | Ha
    | Hi
    | Fu
    | He
    | Ho
    | Ma
    | Mi
    | Mu
    | Me
    | Mo
    | Ya
    | Yu
    | Yo
    | Ra
    | Ri
    | Ru
    | Re
    | Ro
    | Wa
    | Wo
    | N
    | Ga
    | Gi
    | Gu
    | Ge
    | Go
    | Za
    | Ji
    | Zu
    | Ze
    | Zo
    | Da
    | Di
    | Du
    | De
    | Do
    | Ba
    | Bi
    | Bu
    | Be
    | Bo
    | Pa
    | Pi
    | Pu
    | Pe
    | Po


getRandomRomajiList : Int -> Random.Generator (List Romaji)
getRandomRomajiList numQuestions =
    Random.list numQuestions (getRandomRomaji A romajiAsList)


getRandomRomaji : Romaji -> List Romaji -> Random.Generator Romaji
getRandomRomaji default validChoices =
    let
        validChoicesWithoutDefault =
            List.filter (\item -> item /= default) validChoices
    in
    Random.uniform default validChoicesWithoutDefault


englishToList : List String
englishToList =
    List.map (\r -> romajiToEnglish r) romajiAsList


romajiAsList : List Romaji
romajiAsList =
    [ A
    , I
    , U
    , E
    , O
    , Ka
    , Ki
    , Ku
    , Ke
    , Ko
    , Sa
    , Shi
    , Su
    , Se
    , So
    , Ta
    , Chi
    , Tsu
    , Te
    , To
    , Na
    , Ni
    , Nu
    , Ne
    , No
    , Ha
    , Hi
    , Fu
    , He
    , Ho
    , Ma
    , Mi
    , Mu
    , Me
    , Mo
    , Ya
    , Yu
    , Yo
    , Ra
    , Ri
    , Ru
    , Re
    , Ro
    , Wa
    , Wo
    , N
    , Ga
    , Gi
    , Gu
    , Ge
    , Go
    , Za
    , Ji
    , Zu
    , Ze
    , Zo
    , Da
    , Di
    , Du
    , De
    , Do
    , Ba
    , Bi
    , Bu
    , Be
    , Bo
    , Pa
    , Pi
    , Pu
    , Pe
    , Po
    ]


stringToRomaji : String -> Maybe Romaji
stringToRomaji s =
    case s of
        "a" ->
            Just A

        "i" ->
            Just I

        "u" ->
            Just U

        "e" ->
            Just E

        "o" ->
            Just O

        "ka" ->
            Just Ka

        "ki" ->
            Just Ki

        "ku" ->
            Just Ku

        "ke" ->
            Just Ke

        "ko" ->
            Just Ko

        "sa" ->
            Just Sa

        "shi" ->
            Just Shi

        "su" ->
            Just Su

        "se" ->
            Just Se

        "so" ->
            Just So

        "ta" ->
            Just Ta

        "chi" ->
            Just Chi

        "tsu" ->
            Just Tsu

        "te" ->
            Just Te

        "to" ->
            Just To

        "na" ->
            Just Na

        "ni" ->
            Just Ni

        "nu" ->
            Just Nu

        "ne" ->
            Just Ne

        "no" ->
            Just No

        "ha" ->
            Just Ha

        "hi" ->
            Just Hi

        "fu" ->
            Just Fu

        "he" ->
            Just He

        "ho" ->
            Just Ho

        "ma" ->
            Just Ma

        "mi" ->
            Just Mi

        "mu" ->
            Just Mu

        "me" ->
            Just Me

        "mo" ->
            Just Mo

        "ya" ->
            Just Ya

        "yu" ->
            Just Yu

        "yo" ->
            Just Yo

        "ra" ->
            Just Ra

        "ri" ->
            Just Ri

        "ru" ->
            Just Ru

        "re" ->
            Just Re

        "ro" ->
            Just Ro

        "wa" ->
            Just Wa

        "wo" ->
            Just Wo

        "n" ->
            Just N

        "ga" ->
            Just Ga

        "gi" ->
            Just Gi

        "gu" ->
            Just Gu

        "ge" ->
            Just Ge

        "go" ->
            Just Go

        "za" ->
            Just Za

        "ji" ->
            Just Ji

        "zu" ->
            Just Zu

        "ze" ->
            Just Ze

        "zo" ->
            Just Zo

        "da" ->
            Just Da

        "di" ->
            Just Di

        "du" ->
            Just Du

        "de" ->
            Just De

        "do" ->
            Just Do

        "ba" ->
            Just Ba

        "bi" ->
            Just Bi

        "bu" ->
            Just Bu

        "be" ->
            Just Be

        "bo" ->
            Just Bo

        "pa" ->
            Just Pa

        "pi" ->
            Just Pi

        "pu" ->
            Just Pu

        "pe" ->
            Just Pe

        "po" ->
            Just Po

        _ ->
            Nothing


romajiToEnglish : Romaji -> String
romajiToEnglish romaji =
    case romaji of
        A ->
            "a"

        I ->
            "i"

        U ->
            "u"

        E ->
            "e"

        O ->
            "o"

        Ka ->
            "ka"

        Ki ->
            "ki"

        Ku ->
            "ku"

        Ke ->
            "ke"

        Ko ->
            "ko"

        Sa ->
            "sa"

        Shi ->
            "shi"

        Su ->
            "su"

        Se ->
            "se"

        So ->
            "so"

        Ta ->
            "ta"

        Chi ->
            "chi"

        Tsu ->
            "tsu"

        Te ->
            "te"

        To ->
            "to"

        Na ->
            "na"

        Ni ->
            "ni"

        Nu ->
            "nu"

        Ne ->
            "ne"

        No ->
            "no"

        Ha ->
            "ha"

        Hi ->
            "hi"

        Fu ->
            "fu"

        He ->
            "he"

        Ho ->
            "ho"

        Ma ->
            "ma"

        Mi ->
            "mi"

        Mu ->
            "mu"

        Me ->
            "me"

        Mo ->
            "mo"

        Ya ->
            "ya"

        Yu ->
            "yu"

        Yo ->
            "yo"

        Ra ->
            "ra"

        Ri ->
            "ri"

        Ru ->
            "ru"

        Re ->
            "re"

        Ro ->
            "ro"

        Wa ->
            "wa"

        Wo ->
            "wo"

        N ->
            "n"

        Ga ->
            "ga"

        Gi ->
            "gi"

        Gu ->
            "gu"

        Ge ->
            "ge"

        Go ->
            "go"

        Za ->
            "za"

        Ji ->
            "ji"

        Zu ->
            "zu"

        Ze ->
            "ze"

        Zo ->
            "zo"

        Da ->
            "da"

        Di ->
            "di"

        Du ->
            "du"

        De ->
            "de"

        Do ->
            "do"

        Ba ->
            "ba"

        Bi ->
            "bi"

        Bu ->
            "bu"

        Be ->
            "be"

        Bo ->
            "bo"

        Pa ->
            "pa"

        Pi ->
            "pi"

        Pu ->
            "pu"

        Pe ->
            "pe"

        Po ->
            "po"


romajiToHiragana : Romaji -> String
romajiToHiragana romaji =
    case romaji of
        A ->
            "あ"

        I ->
            "い"

        U ->
            "う"

        E ->
            "え"

        O ->
            "お"

        Ka ->
            "か"

        Ki ->
            "き"

        Ku ->
            "く"

        Ke ->
            "け"

        Ko ->
            "こ"

        Sa ->
            "さ"

        Shi ->
            "し"

        Su ->
            "す"

        Se ->
            "せ"

        So ->
            "そ"

        Ta ->
            "た"

        Chi ->
            "ち"

        Tsu ->
            "つ"

        Te ->
            "て"

        To ->
            "と"

        Na ->
            "な"

        Ni ->
            "に"

        Nu ->
            "ぬ"

        Ne ->
            "ね"

        No ->
            "の"

        Ha ->
            "は"

        Hi ->
            "ひ"

        Fu ->
            "ふ"

        He ->
            "へ"

        Ho ->
            "ほ"

        Ma ->
            "ま"

        Mi ->
            "み"

        Mu ->
            "む"

        Me ->
            "め"

        Mo ->
            "も"

        Ya ->
            "や"

        Yu ->
            "ゆ"

        Yo ->
            "よ"

        Ra ->
            "ら"

        Ri ->
            "り"

        Ru ->
            "る"

        Re ->
            "れ"

        Ro ->
            "ろ"

        Wa ->
            "わ"

        Wo ->
            "を"

        N ->
            "ん"

        Ga ->
            "が"

        Gi ->
            "ぎ"

        Gu ->
            "ぐ"

        Ge ->
            "げ"

        Go ->
            "ご"

        Za ->
            "ざ"

        Ji ->
            "じ"

        Zu ->
            "ず"

        Ze ->
            "ぜ"

        Zo ->
            "ぞ"

        Da ->
            "だ"

        Di ->
            "ぢ"

        Du ->
            "づ"

        De ->
            "で"

        Do ->
            "ど"

        Ba ->
            "ば"

        Bi ->
            "び"

        Bu ->
            "ぶ"

        Be ->
            "べ"

        Bo ->
            "ぼ"

        Pa ->
            "ぱ"

        Pi ->
            "ぴ"

        Pu ->
            "ぷ"

        Pe ->
            "ぺ"

        Po ->
            "ぽ"


romajiToOrder : Romaji -> Int
romajiToOrder romaji =
    case romaji of
        A ->
            1

        I ->
            2

        U ->
            3

        E ->
            4

        O ->
            5

        Ka ->
            6

        Ki ->
            7

        Ku ->
            8

        Ke ->
            9

        Ko ->
            10

        Sa ->
            11

        Shi ->
            12

        Su ->
            13

        Se ->
            14

        So ->
            15

        Ta ->
            16

        Chi ->
            17

        Tsu ->
            18

        Te ->
            19

        To ->
            20

        Na ->
            21

        Ni ->
            22

        Nu ->
            23

        Ne ->
            24

        No ->
            25

        Ha ->
            26

        Hi ->
            27

        Fu ->
            28

        He ->
            29

        Ho ->
            30

        Ma ->
            31

        Mi ->
            32

        Mu ->
            33

        Me ->
            34

        Mo ->
            35

        Ya ->
            36

        Yu ->
            37

        Yo ->
            38

        Ra ->
            39

        Ri ->
            40

        Ru ->
            41

        Re ->
            42

        Ro ->
            43

        Wa ->
            44

        Wo ->
            45

        N ->
            46

        Ga ->
            47

        Gi ->
            48

        Gu ->
            49

        Ge ->
            50

        Go ->
            51

        Za ->
            52

        Ji ->
            53

        Zu ->
            54

        Ze ->
            55

        Zo ->
            56

        Da ->
            57

        Di ->
            58

        Du ->
            59

        De ->
            60

        Do ->
            61

        Ba ->
            62

        Bi ->
            63

        Bu ->
            64

        Be ->
            65

        Bo ->
            66

        Pa ->
            67

        Pi ->
            68

        Pu ->
            69

        Pe ->
            70

        Po ->
            71
