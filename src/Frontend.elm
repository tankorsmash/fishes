module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Color
import Color.Convert
import Color.Manipulate
import Direction2d
import Duration
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, modular, padding, paddingXY, paragraph, px, rgb, rgb255, row, scrollbars, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import Html
import Html.Attributes
import Lamdera
import Length
import List.Extra
import Pixels
import Point2d exposing (pixels)
import Random
import Time
import Types exposing (..)
import Url
import Vector2d


type alias Model =
    FrontendModel


type alias Msg =
    FrontendMsg


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions =
            \model ->
                if model.isPaused then
                    Sub.none

                else
                    Time.every (1000 / 60) GameTick
        , view = viewWrapper
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        firstTickTime =
            Time.millisToPosix 0
    in
    ( { key = key
      , isPaused = False
      , lastTickTime = firstTickTime
      , deltaTime = 0
      , globalSeed = Random.initialSeed 0
      , fishes =
            let
                newFish =
                    initFish firstTickTime
            in
            [ { newFish | pos = pixels 50 222, id = 1 }
            , { newFish | pos = pixels 199 20, id = 2 }
            ]
      , coinsInPlay = []
      , coinsCollected = 0
      }
    , Cmd.none
    )


type alias Size =
    { w : Int, h : Int }


fishSize : Size
fishSize =
    { w = 70, h = 40 }


coinSize : Size
coinSize =
    { w = 40, h = 40 }


aquariumSize : Size
aquariumSize =
    { w = 720, h = 500 }


convertColor : Color.Color -> Element.Color
convertColor color =
    Element.fromRgb <| Color.toRgba <| color


colorFromInt : Int -> Color -> Color -> Color -> Color
colorFromInt int positiveColor neutralColor negativeColor =
    if int > 0 then
        positiveColor

    else if int == 0 then
        neutralColor

    else
        negativeColor


monospace : List (Element.Attribute msg) -> Element msg -> Element msg
monospace attrs el =
    Element.el (Font.family [ Font.monospace ] :: attrs) el


noUserSelect : Element.Attribute msg
noUserSelect =
    Html.Attributes.style "userSelect" "none" |> Element.htmlAttribute


noopAttr : Element.Attribute msg
noopAttr =
    Html.Attributes.style "" "" |> Element.htmlAttribute


viewCoin : Coin -> Element Msg
viewCoin coin =
    let
        backgroundColor : Color.Color
        backgroundColor =
            Color.yellow

        coinPos =
            .pos coin |> Point2d.toPixels

        coinX =
            coinPos.x - (.w coinSize |> toFloat >> (\w -> w / 2))

        coinY =
            coinPos.y - (.h coinSize |> toFloat >> (\h -> h / 2))
    in
    Keyed.el
        [ width <| px (.w coinSize)
        , height <| px (.h coinSize)
        , Background.color <| convertColor <| backgroundColor
        , Border.rounded <| 10
        , Element.moveRight coinX
        , Element.moveDown coinY
        , Events.onClick (CollectCoin coin.id)
        , Element.pointer
        , Element.mouseOver [ Background.color (backgroundColor |> Color.Manipulate.lighten 0.1 |> convertColor) ]
        , noUserSelect
        ]
    <|
        ( String.fromInt coin.id
        , text ""
        )


viewFishEye : Bool -> Element Msg
viewFishEye isAlive =
    let
        iris =
            if isAlive then
                el
                    [ width <| px 5
                    , height <| px 5
                    , Border.rounded 5
                    , Background.color <| convertColor Color.black
                    , alignRight
                    , Element.moveDown 5
                    , Element.moveRight -1
                    ]
                <|
                    text ""

            else
                --dead iris
                el
                    [ Font.size 16
                    , centerX
                    , centerY
                    , width fill
                    , height fill
                    , Font.center
                    ]
                <|
                    text "x"
    in
    el
        [ width <| px 15
        , height <| px 15
        , Border.rounded 5
        , Background.color <| convertColor Color.white
        , alignRight
        , Element.moveDown 5
        , Element.inFront iris
        ]
    <|
        text ""


viewFish : Time.Posix -> Fish -> Element Msg
viewFish lastTickTime fish =
    let
        fishPos =
            .pos fish |> Point2d.toPixels

        fishX =
            fishPos.x - (.w fishSize |> toFloat >> (\w -> w / 2))

        fishY =
            fishPos.y - (.h fishSize |> toFloat >> (\h -> h / 2))

        hungerStatus =
            getHungerStatus lastTickTime fish.hunger

        isNotHungry =
            hungerStatus == NotHungry

        backgroundColor : Color.Color
        backgroundColor =
            case hungerStatus of
                Starved ->
                    Color.rgb255 0x0A 0x0A 0x0A

                Starving ->
                    Color.Manipulate.darken 0.35 Color.red

                VeryHungry ->
                    Color.Manipulate.darken 0.25 Color.red

                Hungry ->
                    Color.Manipulate.darken 0.2 Color.green

                NotHungry ->
                    Color.Manipulate.darken 0.0 Color.green
    in
    Keyed.el
        [ width <| px (.w fishSize)
        , height <| px (.h fishSize)
        , Background.color <| convertColor <| backgroundColor
        , Border.rounded <| 10
        , Element.moveRight fishX
        , Element.moveDown fishY
        , Events.onClick <|
            if not isNotHungry then
                FeedFish fish.id

            else
                NoOpFrontendMsg
        , if not isNotHungry then
            Element.pointer

          else
            noopAttr
        , Element.mouseOver <|
            if not isNotHungry then
                [ Background.color <|
                    (backgroundColor
                        |> Color.Manipulate.lighten 0.1
                        |> convertColor
                    )
                ]

            else
                []
        , noUserSelect
        , Element.inFront <|
            viewFishEye (hungerStatus /= Starved)
        ]
    <|
        ( String.fromInt fish.id
        , text ""
        )


viewFishes : Time.Posix -> List Fish -> List Coin -> Element Msg
viewFishes lastTickTime fishes coins =
    column
        ([ centerX
         , width <| px <| .w aquariumSize
         , height <| px <| .h aquariumSize
         , Element.clip
         , Background.color <| rgb255 28 163 236
         , noUserSelect
         ]
            ++ List.map (Element.inFront << viewFish lastTickTime) fishes
            ++ List.map (Element.inFront << viewCoin) coins
        )
    <|
        [ text "" ]


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GameTick newTime ->
            let
                newModel =
                    { model | lastTickTime = newTime }

                deltaTime =
                    Time.posixToMillis newTime - Time.posixToMillis (.lastTickTime model)
            in
            -- skip update since we havent gotten a first tick yet (lamdera limitation afaict)
            if Time.posixToMillis (.lastTickTime model) == 0 then
                onFirstFrame newModel

            else
                onGameTick { newModel | deltaTime = deltaTime }

        FeedFish fishId ->
            let
                newFish =
                    List.Extra.updateIf
                        (\fish -> fish.id == fishId)
                        (\fish -> { fish | hunger = Sated model.lastTickTime })
                        model.fishes
            in
            ( { model | fishes = newFish }, Cmd.none )

        CollectCoin coinId ->
            let
                newCoin =
                    List.filter
                        (\coin -> coin.id /= coinId)
                        model.coinsInPlay
            in
            ( { model
                | coinsInPlay = newCoin
                , coinsCollected = model.coinsCollected + 1
              }
            , Cmd.none
            )

        BuyFish ->
            let
                _ =
                    123
            in
            ( if canAffordBuyingFish model.coinsCollected then
                let
                    ( newFish, newSeed ) =
                        Random.step (generateFish model.lastTickTime) model.globalSeed
                in
                { model
                    | coinsCollected = model.coinsCollected - priceOfFish
                    , fishes = newFish :: model.fishes
                    , globalSeed = newSeed
                }

              else
                model
            , Cmd.none
            )

        TogglePause ->
            ( { model | isPaused = not model.isPaused }, Cmd.none )


generateCoinId : Random.Generator Int
generateCoinId =
    Random.int 0 100000000


generateFishId : Random.Generator Int
generateFishId =
    Random.int 0 100000000


generateFishPos : Random.Generator Pixel2i
generateFishPos =
    Random.map2
        (\xi yi ->
            pixels (toFloat xi) (toFloat yi)
        )
        (Random.int 0 aquariumSize.w)
        (Random.int 0 aquariumSize.h)


generateFish : Time.Posix -> Random.Generator Fish
generateFish lastTickTime =
    let
        newFish =
            initFish lastTickTime
    in
    Random.map2
        (\id pos ->
            { newFish
                | id = id
                , pos = pos
            }
        )
        generateFishId
        generateFishPos


priceOfFish : Int
priceOfFish =
    5


canAffordBuyingFish : Int -> Bool
canAffordBuyingFish coinsCollected =
    coinsCollected >= priceOfFish


onFirstFrame : Model -> ( Model, Cmd Msg )
onFirstFrame model =
    let
        newFish =
            model.fishes
                |> List.map
                    (\fish ->
                        { fish | hunger = Sated model.lastTickTime }
                    )
    in
    ( { model | fishes = newFish }, Cmd.none )


sinceEaten : Time.Posix -> FishHunger -> Duration.Duration
sinceEaten lastTickTime (Sated lastEaten) =
    (Time.posixToMillis lastTickTime - Time.posixToMillis lastEaten)
        |> toFloat
        |> Duration.milliseconds


type HungerStatus
    = NotHungry
    | Hungry
    | VeryHungry
    | Starving
    | Starved --aka dead


isHungry : Time.Posix -> FishHunger -> Bool
isHungry lastTickTime hunger =
    let
        secondsSinceEaten =
            sinceEaten lastTickTime hunger |> Duration.inSeconds

        secondsLimit =
            Duration.seconds 5 |> Duration.inSeconds
    in
    secondsSinceEaten > secondsLimit


sec : Float -> Float
sec n =
    Duration.seconds n |> Duration.inSeconds


getHungerStatus : Time.Posix -> FishHunger -> HungerStatus
getHungerStatus lastTickTime hunger =
    let
        secondsSinceEaten =
            sinceEaten lastTickTime hunger |> Duration.inSeconds
    in
    if secondsSinceEaten >= sec 30 then
        Starved

    else if secondsSinceEaten >= sec 25 then
        Starving

    else if secondsSinceEaten >= sec 15 then
        VeryHungry

    else if secondsSinceEaten >= sec 10 then
        Hungry

    else
        NotHungry


moveFish : HungerStatus -> Random.Seed -> Pixel2i -> ( Pixel2i, Random.Seed )
moveFish hungerStatus seed pos =
    let
        isSated =
            hungerStatus == NotHungry

        moveGen =
            Random.int 0 1000

        rareMoveForward =
            Random.int 0 2 |> Random.map (\right -> pixelsRight (toFloat right) pos)

        standardMoveUpDown =
            let
                pix =
                    Point2d.toPixels pos
            in
            --if fish is close to bottom of aquarium, move it up
            (if round pix.y >= (aquariumSize.h - fishSize.h) then
                Random.int -5 -1

             else
                Random.int -5 5
            )
                |> Random.map (\down -> pixelsDown (toFloat down) pos)
    in
    if isSated then
        Random.step
            (moveGen
                |> Random.andThen
                    (\doIt ->
                        if doIt <= 5 then
                            rareMoveForward

                        else if doIt <= 15 then
                            standardMoveUpDown

                        else
                            Random.constant pos
                    )
            )
            seed

    else if hungerStatus == Starving then
        Random.step
            (Random.int 0 10
                |> Random.andThen
                    (\doIt ->
                        if doIt <= 9 then
                            Random.map (\down -> pixelsDown (toFloat down) pos)
                                (Random.int 0 3)

                        else
                            Random.constant pos
                    )
            )
            seed

    else if hungerStatus == Starved then
        Random.step
            (Random.int 0 10
                |> Random.andThen
                    (\doIt ->
                        if doIt <= 9 then
                            Random.map (\down -> pixelsDown (toFloat down) pos)
                                (Random.int 0 2)

                        else
                            Random.constant pos
                    )
            )
            seed

    else
        ( pos, seed )


constrainFishBounds : Pixel2i -> Pixel2i
constrainFishBounds fishPos =
    let
        pix =
            Point2d.toPixels fishPos
    in
    if round pix.x >= (aquariumSize.w - (fishSize.w // 2)) then
        -- too far right
        Point2d.fromPixels { pix | x = toFloat (fishSize.w // 2) }

    else if round pix.y >= (aquariumSize.h - (fishSize.h // 2)) then
        -- too far down
        Point2d.fromPixels { pix | y = toFloat (aquariumSize.h - (fishSize.h // 2)) }

    else if round pix.y <= (fishSize.h // 2) then
        -- too far left
        Point2d.fromPixels { pix | y = toFloat (fishSize.h // 2) }

    else
        fishPos


updateFish : Time.Posix -> Fish -> ( List Fish, Random.Seed, List Coin ) -> ( List Fish, Random.Seed, List Coin )
updateFish lastTickTime fish ( fishes, seed, coins ) =
    let
        hungerStatus =
            getHungerStatus lastTickTime fish.hunger

        isSated =
            hungerStatus == NotHungry

        newPos : Pixel2i -> Random.Seed -> ( Pixel2i, Random.Seed )
        newPos oldPos s =
            oldPos
                |> pixelsRight
                    (case hungerStatus of
                        Starved ->
                            0.0

                        Starving ->
                            0.5

                        VeryHungry ->
                            0.85

                        Hungry ->
                            1.5

                        NotHungry ->
                            2.5
                    )
                |> moveFish hungerStatus s
                |> Tuple.mapFirst constrainFishBounds

        ( newCoins_, newSeed ) =
            if isSated then
                let
                    ( ( shouldSpawnCoin, coinId ), newSeed_ ) =
                        Random.step
                            (Random.map2 Tuple.pair
                                generateShouldSpawnCoin
                                generateCoinId
                            )
                            seed
                in
                if shouldSpawnCoin then
                    let
                        newCoin =
                            initCoin lastTickTime
                                |> (\c ->
                                        { c
                                            | pos = fish.pos |> pixelsDown (fishSize.h // 2 + (coinSize.h // 2) |> toFloat)
                                            , id = coinId
                                        }
                                   )
                    in
                    ( newCoin :: coins, newSeed_ )

                else
                    ( coins, newSeed_ )

            else
                ( coins, seed )

        ( newestPos, newestSeed ) =
            newPos fish.pos newSeed
    in
    ( { fish | pos = newestPos } :: fishes, newestSeed, newCoins_ )


generateShouldSpawnCoin : Random.Generator Bool
generateShouldSpawnCoin =
    Random.int 0 600
        |> Random.andThen
            (\res ->
                Random.constant (res == 0)
            )


pixelsDown : Float -> Pixel2i -> Pixel2i
pixelsDown down =
    Point2d.translateIn Direction2d.y (Pixels.pixels down)


pixelsRight : Float -> Pixel2i -> Pixel2i
pixelsRight right =
    Point2d.translateIn Direction2d.x (Pixels.pixels right)


updateCoin : Time.Posix -> Coin -> ( List Coin, Random.Seed ) -> ( List Coin, Random.Seed )
updateCoin lastTickTime coin ( movedCoins, seed ) =
    let
        newPos =
            coin.pos
                |> pixelsDown 1
                |> (\np ->
                        let
                            pix =
                                Point2d.toPixels np
                        in
                        if round pix.y >= (aquariumSize.h - coinSize.h // 2) then
                            Point2d.fromPixels { pix | y = toFloat <| aquariumSize.h - coinSize.h // 2 }

                        else
                            np
                   )
    in
    if (timeSinceCoinCreated lastTickTime coin |> Duration.inSeconds) < sec 30 then
        ( { coin | pos = newPos } :: movedCoins, seed )

    else
        ( movedCoins, seed )


timeSinceCoinCreated : Time.Posix -> Coin -> Duration.Duration
timeSinceCoinCreated lastTickTime { createdAt } =
    (Time.posixToMillis lastTickTime - Time.posixToMillis createdAt)
        |> toFloat
        |> Duration.milliseconds


onGameTick : Model -> ( Model, Cmd Msg )
onGameTick model =
    let
        ( newFishes, newSeed_, newCoins_ ) =
            List.foldl
                (updateFish model.lastTickTime)
                ( [], model.globalSeed, [] )
                model.fishes

        ( newCoins, newSeed ) =
            List.foldl
                (updateCoin model.lastTickTime)
                ( [], newSeed_ )
                (model.coinsInPlay ++ newCoins_)
    in
    ( { model
        | fishes = List.reverse newFishes
        , globalSeed = newSeed
        , coinsInPlay = List.reverse newCoins
      }
    , Cmd.none
    )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


viewWrapper : Model -> Browser.Document FrontendMsg
viewWrapper model =
    let
        elm_ui_hack_layout =
            Html.div [ Html.Attributes.style "height" "0" ]
                [ Element.layoutWith
                    { options =
                        [ Element.focusStyle
                            { borderColor = Nothing
                            , backgroundColor = Nothing
                            , shadow = Nothing
                            }
                        ]
                    }
                    [ Element.htmlAttribute <| Html.Attributes.id "hack" ]
                  <|
                    Element.none
                ]
    in
    { title = "Fishes"
    , body =
        [ elm_ui_hack_layout
        , Element.layoutWith
            { options =
                [ Element.noStaticStyleSheet
                , Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ width fill
            , height fill
            , Element.htmlAttribute <| Html.Attributes.id "elm_ui_layout"
            , centerX
            ]
          <|
            view model
        ]
    }


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


viewDebugRow : Model -> Element msg
viewDebugRow model =
    row [ centerX, spacing 10, Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 } ] <|
        [ el [ Font.size <| round <| scaled 2, centerY ] <| text "Debug: "
        , el [ Font.size <| round <| scaled 1, centerY ] <| text <| "Coins in play: " ++ (String.fromInt <| List.length model.coinsInPlay)
        , el [ Font.size <| round <| scaled 1, centerY ] <| text <| "Frametime: " ++ (String.fromInt <| model.deltaTime) ++ "ms"
        ]


viewCommandRow : Model -> Element FrontendMsg
viewCommandRow model =
    row [ centerX, spacing 10 ] <|
        [ el [ Font.size <| round <| scaled 1 ] <| text <| "Coins collected: " ++ String.fromInt model.coinsCollected
        , Input.button
            [ padding 5
            , Border.rounded 5
            , Border.width 2
            , Border.color <| convertColor colors.darkButtonColor
            , Background.color <| convertColor colors.lightButtonColor
            , Element.mouseOver
                [ Background.color <| convertColor colors.highlightButtonColor
                , Border.color <| convertColor colors.lightButtonColor
                ]
            ]
            { onPress = Just BuyFish
            , label = text "Buy Fish"
            }
        , Input.button
            [ padding 5
            , Border.rounded 5
            , Border.width 2
            , Border.color <| convertColor colors.darkButtonColor
            , Background.color <| convertColor colors.lightButtonColor
            , Element.mouseOver
                [ Background.color <| convertColor colors.highlightButtonColor
                , Border.color <| convertColor colors.lightButtonColor
                ]
            ]
            { onPress = Just TogglePause
            , label =
                text <|
                    if model.isPaused then
                        "Resume"

                    else
                        "Pause Anims"
            }
        ]


view : Model -> Element FrontendMsg
view model =
    column [ width fill, height fill ]
        [ paragraph [ centerX, width fill, Font.center, padding 10 ]
            [ text "Welcome to "
            , el [ Font.underline, Font.bold ] <| text "Fishes"
            , text "!"
            ]
        , viewFishes model.lastTickTime model.fishes model.coinsInPlay
        , column [ width fill, height fill, spacing 10 ]
            [ viewCommandRow model
            , viewDebugRow model
            ]
        ]


getRawColorFromHex : String -> Color.Color
getRawColorFromHex hexStr =
    Color.Convert.hexToColor hexStr
        |> Result.withDefault (Color.rgb 1 0 1)


colors : { buttonColor : Color.Color, darkButtonColor : Color.Color, lightButtonColor : Color.Color, highlightButtonColor : Color.Color, offwhiteButtonColor : Color.Color }
colors =
    { darkButtonColor = getRawColorFromHex "#012E4A"
    , buttonColor = getRawColorFromHex "#036280"
    , lightButtonColor = getRawColorFromHex "#378BA4"
    , highlightButtonColor = getRawColorFromHex "#81BECE"
    , offwhiteButtonColor = getRawColorFromHex "#E8EDE7"
    }
