module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Dict exposing (Dict)

type alias PlayerId = Int
type alias GameId = Int

type alias Player =
    { id : PlayerId
    , displayName : String
    }

type alias GameData =
    { id : GameId
    , players : List Player
    }

type FrontendState
    = BrowsingGames FrontendGameList
    | ConnectingToGame GameId
    | InGame GameId

type alias FrontendModel =
    { key : Key -- used by Browser.Navigation for things like pushUrl
    , state : FrontendState
    }

type alias BackendModel =
    { games : Dict Int GameData
    , gameIdNonce : GameId -- the id that will be assigned to the next created game
    , playerIdNonce : PlayerId -- the id that will be assigned to the next created player
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