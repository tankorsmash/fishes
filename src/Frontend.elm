module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Color
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
        , subscriptions = \model -> Time.every (1000 / 60) GameTick
        , view = viewWrapper
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
      , lastTickTime = Time.millisToPosix 0
      , deltaTime = 0
      , globalSeed = Random.initialSeed 0
      , fishes =
            [ { initFish | pos = pixels 50 222, id = 1 }
            , { initFish | pos = pixels 199 20, id = 2 }
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
        , monospace [ centerX, centerY ] <|
            text <|
                String.fromInt coin.id
        )


viewFish : Time.Posix -> Fish -> Element Msg
viewFish lastTickTime fish =
    let
        fishPos =
            .pos fish |> Point2d.toPixels

        fishX =
            fishPos.x - (.w fishSize |> toFloat >> (\w -> w / 2))

        fishY =
            fishPos.y - (.h fishSize |> toFloat >> (\h -> h / 2))

        backgroundColor : Color.Color
        backgroundColor =
            if isHungry lastTickTime fish.hunger then
                Color.red

            else
                Color.green

        roundNumber : String -> String
        roundNumber str =
            String.padLeft 3 '0' str
    in
    Keyed.el
        [ width <| px (.w fishSize)
        , height <| px (.h fishSize)
        , Background.color <| convertColor <| backgroundColor
        , Border.rounded <| 10
        , Element.moveRight fishX
        , Element.moveDown fishY
        , Events.onClick (FeedFish fish.id)
        , Element.pointer
        , Element.mouseOver
            [ Background.color <|
                (backgroundColor
                    |> Color.Manipulate.lighten 0.1
                    |> convertColor
                )
            ]
        , noUserSelect
        ]
    <|
        ( String.fromInt fish.id
        , monospace [ centerX, centerY ] <|
            text <|
                let
                    prettyPos : ({ x : Float, y : Float } -> Float) -> String
                    prettyPos getter =
                        .pos fish
                            |> Point2d.toPixels
                            |> getter
                            |> round
                            |> String.fromInt
                            |> roundNumber

                    xPos =
                        prettyPos .x

                    yPos =
                        prettyPos .y
                in
                xPos ++ ", " ++ yPos
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


isHungry : Time.Posix -> FishHunger -> Bool
isHungry lastTickTime hunger =
    let
        secondsSinceEaten =
            sinceEaten lastTickTime hunger |> Duration.inSeconds

        secondsLimit =
            Duration.seconds 5 |> Duration.inSeconds
    in
    secondsSinceEaten > secondsLimit


moveFishVerticallyIfSated : Bool -> Random.Seed -> Pixel2i -> ( Pixel2i, Random.Seed )
moveFishVerticallyIfSated isSated seed pos =
    if isSated then
        Random.step
            (Random.int 0 1000
                |> Random.andThen
                    (\doIt ->
                        if doIt <= 5 then
                            Random.int 0 2
                                |> Random.map (\right -> pixelsRight (toFloat right) pos)

                        else if doIt <= 15 then
                            Random.int -5 5
                                |> Random.map (\down -> pixelsDown (toFloat down) pos)

                        else
                            Random.constant pos
                    )
            )
            seed

    else
        ( pos, seed )


moveFish : Time.Posix -> Fish -> ( List Fish, Random.Seed, List Coin ) -> ( List Fish, Random.Seed, List Coin )
moveFish lastTickTime fish ( fishes, seed, coins ) =
    let
        isSated =
            not <| isHungry lastTickTime fish.hunger

        newPos : Pixel2i -> Random.Seed -> ( Pixel2i, Random.Seed )
        newPos oldPos s =
            oldPos
                |> pixelsRight (if isSated then 1.5 else 1)
                |> moveFishVerticallyIfSated isSated s
                |> Tuple.mapFirst
                    (\np ->
                        let
                            pix =
                                Point2d.toPixels np
                        in
                        if round pix.x >= (aquariumSize.w - (fishSize.w //2)) then
                            Point2d.fromPixels { pix | x = toFloat (fishSize.w // 2) }

                        else if round pix.y >= aquariumSize.h then
                            Point2d.fromPixels { pix | y = toFloat aquariumSize.h }

                        else if round pix.y < 0 then
                            Point2d.fromPixels { pix | y = toFloat (fishSize.h // 2) }

                        else
                            np
                    )

        ( newCoins_, newSeed ) =
            if isSated then
                let
                    ( ( res, coinId ), newSeed_ ) =
                        Random.step
                            (Random.map2 Tuple.pair
                                (Random.int 0 600)
                                (Random.int 0 100000000)
                            )
                            seed

                    shouldSpawnCoin =
                        res <= 2
                in
                if shouldSpawnCoin then
                    let
                        newCoin =
                            { initCoin
                                | pos = fish.pos |> pixelsDown (fishSize.h // 2 + (coinSize.h // 2) |> toFloat)
                                , id = coinId
                            }
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


pixelsDown : Float -> Pixel2i -> Pixel2i
pixelsDown down =
    Point2d.translateIn Direction2d.y (Pixels.pixels down)


pixelsRight : Float -> Pixel2i -> Pixel2i
pixelsRight right =
    Point2d.translateIn Direction2d.x (Pixels.pixels right)


moveCoin : Coin -> ( List Coin, Random.Seed ) -> ( List Coin, Random.Seed )
moveCoin coin ( movedCoins, seed ) =
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
    ( { coin | pos = newPos } :: movedCoins, seed )


onGameTick : Model -> ( Model, Cmd Msg )
onGameTick model =
    let
        ( newFishes, newSeed_, newCoins_ ) =
            List.foldl
                (moveFish model.lastTickTime)
                ( [], model.globalSeed, [] )
                model.fishes

        ( newCoins, newSeed ) =
            List.foldl
                moveCoin
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


viewDebugRow model =
    row [ centerX, spacing 10, Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 } ] <|
        [ el [ Font.size <| round <| scaled 2, centerY ] <| text "Debug: "
        , el [ Font.size <| round <| scaled 1, centerY ] <| text "Fed XYZ Times"
        , el [ Font.size <| round <| scaled 1, centerY ] <| text <| "Coins in play: " ++ (String.fromInt <| List.length model.coinsInPlay)
        , el [ Font.size <| round <| scaled 1, centerY ] <| text <| "Frametime: " ++ (String.fromInt <| model.deltaTime) ++ "ms"
        ]


view : Model -> Element FrontendMsg
view model =
    column [ width fill, height fill ]
        [ el [ centerX ] <| text "Welcome to Fishes"
        , viewFishes model.lastTickTime model.fishes model.coinsInPlay
        , viewDebugRow model
        , row [ centerX, spacing 10 ] <|
            [ el [ Font.size <| round <| scaled 1 ] <| text <| "Coins collected: " ++ String.fromInt model.coinsCollected
            ]
        ]
