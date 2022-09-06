module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.Pixels
import Evergreen.V1.Point2d
import Random
import Time
import Url


type alias FishId = Int


type alias Pixel2i = (Evergreen.V1.Point2d.Point2d Evergreen.V1.Pixels.Pixels Int)


type FishSize
    = SmallFish
    | MedFish
    | LargeFish


type FishHunger
    = Sated Time.Posix


type alias Fish = 
    { id : FishId
    , pos : Pixel2i
    , size : FishSize
    , hunger : FishHunger
    }


type alias CoinId = Int


type alias Coin = 
    { id : CoinId
    , pos : Pixel2i
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , message : String
    , fishes : (List Fish)
    , coinsInPlay : (List Coin)
    , coinsCollected : Int
    , lastTickTime : Time.Posix
    , deltaTime : Int
    , globalSeed : Random.Seed
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | GameTick Time.Posix
    | FeedFish FishId
    | CollectCoin FishId
    | BuyFish


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend