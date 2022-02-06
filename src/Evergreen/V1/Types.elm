module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Url


type alias GameId =
    Int


type FrontendState
    = BrowsingGames String
    | ConnectingToGame GameId
    | InGame GameId


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , state : FrontendState
    , hasBeenGreeted : Bool
    }


type alias PlayerId =
    Int


type alias Player =
    { id : PlayerId
    , displayName : String
    }


type alias GameState =
    { id : GameId
    , joinCode : String
    , players : List Player
    }


type alias BackendModel =
    { games : Dict.Dict Int GameState
    , players : Dict.Dict Int Player
    , gameIdNonce : GameId
    , playerIdNonce : PlayerId
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend
    | CreateGame
    | JoinGame String


type BackendMsg
    = NoOpBackendMsg
    | OnConnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | GameJoined GameState
    | Greeting
