module Types exposing (..)

import Random
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Browser.Dom
import Pixels
import Point2d
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , isPaused : Bool
    , fishes : List Fish
    , coinsInPlay : List Coin
    , coinsCollected : Int
    , lastTickTime : Time.Posix
    , deltaTime : Int
    , globalSeed : Random.Seed
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

type alias CoinId = Int

type alias Coin =
    { id : CoinId
    , pos : Pixel2i
    , createdAt : Time.Posix
    }

type alias Fish =
    { id : FishId
    , pos : Pixel2i
    , size : FishSize
    , hunger : FishHunger
    }


initFish : Time.Posix -> Fish
initFish lastTickTime =
    { id = 0
    , pos = Point2d.pixels 0 0
    , size = SmallFish
    , hunger = Sated lastTickTime
    }

initCoin : Time.Posix -> Coin
initCoin createdAt =
    { id = 0
    , pos = Point2d.pixels 0 0
    , createdAt = createdAt
    }


type alias FishId = Int

type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GameTick Time.Posix
    | FeedFish FishId
    | RemoveDeadFish FishId
    | CollectCoin FishId
    | BuyFish
    | TogglePause
    | ClickAquarium Pixel2i


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
