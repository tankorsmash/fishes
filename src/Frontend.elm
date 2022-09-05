module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Color
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, modular, padding, paddingXY, paragraph, px, rgb, rgb255, row, scrollbars, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Html
import Html.Attributes
import Lamdera
import Point2d exposing (pixels)
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
      , fishes =
            [ { initFish | pos = pixels 50 222 }
            , { initFish | pos = pixels 199 20 }
            ]
      }
    , Cmd.none
    )


type alias Size =
    { w : Int, h : Int }


fishSize : Size
fishSize =
    { w = 70, h = 20 }


aquariumSize : Size
aquariumSize =
    { w = 720, h = 500 }


viewFish : Fish -> Element Msg
viewFish fish =
    let
        fishPos =
            .pos fish |> Point2d.toPixels

        fishX =
            fishPos.x - (.w fishSize |> toFloat >> (\w -> w / 2))

        fishY =
            fishPos.y - (.h fishSize |> toFloat >> (\h -> h / 2))
    in
    el
        [ width <| px (.w fishSize)
        , height <| px (.h fishSize)
        , Background.color <| rgb 1 0 1
        , Border.rounded <| 10
        , Element.moveRight fishX
        , Element.moveDown fishY
        ]
    <|
        text <|
            String.fromFloat (.pos fish |> Point2d.toPixels |> .x)
                ++ ", "
                ++ String.fromFloat (.pos fish |> Point2d.toPixels |> .y)


viewFishes : List Fish -> Element Msg
viewFishes fishes =
    column
        ([ centerX
         , width <| px <| .w aquariumSize
         , height <| px <| .h aquariumSize
         , Element.clip
         , Background.color <| rgb255 28 163 236
         ]
            ++ List.map (Element.inFront << viewFish) fishes
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
                ( newModel, Cmd.none )

            else
                onGameTick newModel deltaTime


onGameTick : Model -> Int -> ( Model, Cmd Msg )
onGameTick model deltaTime =
    let
        moveFish fish =
            let
                newPos =
                    Point2d.translateBy (Vector2d.pixels 2 0) fish.pos
                        |> (\np ->
                                let
                                    pix =
                                        Point2d.toPixels np
                                in
                                if round pix.x >= aquariumSize.w then
                                    Point2d.fromPixels { pix | x = 0 }

                                else
                                    np
                           )
            in
            { fish | pos = newPos }
    in
    ( { model | fishes = List.map moveFish model.fishes }, Cmd.none )


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


view : Model -> Element FrontendMsg
view model =
    column [ width fill, height fill ]
        [ el [ centerX ] <| text "Welcome to Fishes"
        , viewFishes model.fishes
        ]