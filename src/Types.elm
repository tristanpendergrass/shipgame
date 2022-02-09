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
    , displayName : Maybe String
    }


type alias GameState =
    { id : GameId
    , joinCode : String -- the code that players can use to join the game
    , players : List Lamdera.ClientId
    , unnamedPlayers : List Lamdera.ClientId
    }


type FrontendState
    = OutOfGame String
    | ConnectingToGame
    | NamingPlayer String GameState
    | EnteringLobby GameState
    | InGame GameState


type alias FrontendModel =
    { key : Key -- used by Browser.Navigation for things like pushUrl
    , state : FrontendState
    }


type alias BackendModel =
    { games : Dict GameId GameState
    , players : Dict Lamdera.ClientId Player
    , gameIdNonce : GameId -- the id that will be assigned to the next created game
    , playerIdNonce : PlayerId -- the id that will be assigned to the next created player
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | HandleJoinCodeInput String
    | HandleJoinCodeSubmit
    | HandleCreateGameButtonClick
    | HandleNameInput String
    | HandleNameSubmit


type ToBackend
    = NoOpToBackend
    | CreateGame
    | JoinGame String
    | NamePlayer GameId String


type BackendMsg
    = NoOpBackendMsg
    | HandleConnect Lamdera.SessionId Lamdera.ClientId
    | HandleDisconnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | UpdateGame GameState
