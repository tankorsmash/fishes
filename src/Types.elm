module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Pixels
import Point2d
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , fishes : List Fish
    , lastTickTime : Time.Posix
    }


type alias BackendModel =
    { message : String
    }


type FishSize
    = SmallFish
    | MedFish
    | LargeFish


type alias Pixel2i =
    Point2d.Point2d Pixels.Pixels Int


type FishHunger
    = Sated Time.Posix


type alias Fish =
    { id : Int
    , pos : Pixel2i
    , size : FishSize
    , hunger : FishHunger
    }


initFish : Fish
initFish =
    { id = 0
    , pos = Point2d.pixels 0 0
    , size = SmallFish
    , hunger = Sated (Time.millisToPosix 0)
    }


type alias FishId = Int

type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GameTick Time.Posix
    | FeedFish FishId


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
