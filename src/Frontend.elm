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
import Types exposing (..)
import Url


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
        , subscriptions = \m -> Sub.none
        , view = viewWrapper
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
      , fishes =
            [ { initFish | pos = pixels 50 222 }
            , { initFish | pos = pixels 299 20 }
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
            .x fishPos - (.w fishSize |> toFloat >> (\w -> w / 2))

        fishY =
            .y fishPos - (.h fishSize |> toFloat >> (\h -> h / 2))
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
         , Background.color <| rgb255 28 163 236
         ]
            ++ List.map (Element.inFront << viewFish) fishes
        )
    <|
        [ text "" ]


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
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
