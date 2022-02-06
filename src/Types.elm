module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera
import Set exposing (Set)
import Url exposing (Url)


type alias PlayerId =
    Int


type alias GameId =
    Int


type alias Player =
    { id : PlayerId
    , displayName : String
    }


type alias GameState =
    { id : GameId
    , joinCode : String -- the code that players can use to join the game
    , players : List Player
    }


type FrontendState
    = BrowsingGames String
    | ConnectingToGame GameId
    | InGame GameId


type alias FrontendModel =
    { key : Key -- used by Browser.Navigation for things like pushUrl
    , state : FrontendState
    , hasBeenGreeted : Bool
    }


type alias BackendModel =
    { games : Dict Int GameState
    , players : Dict Int Player
    , gameIdNonce : GameId -- the id that will be assigned to the next created game
    , playerIdNonce : PlayerId -- the id that will be assigned to the next created player
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
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
