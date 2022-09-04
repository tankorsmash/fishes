module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Point2d
import Url exposing (Url)
import Pixels


type alias FrontendModel =
    { key : Key
    , message : String
    , fishes : List Fish
    }


type alias BackendModel =
    { message : String
    }


type alias Pixel2i = Point2d.Point2d Pixels.Pixels Int

type alias Fish =
    { id : Int
    , pos : Pixel2i
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
