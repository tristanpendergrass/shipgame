module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera
import Player exposing (Player, PlayerId)
import Random
import Set exposing (Set)
import ShipGame exposing (ShipGame)
import Url exposing (Url)


type alias LobbyId =
    Int


type alias Lobby =
    { id : LobbyId
    , joinCode : String -- the code that players can use to join the game
    , waitingRoom : Dict PlayerId Player -- players that don't have a name yet
    , game : ShipGame
    }


type FrontendState
    = Unconnected
    | MainMenu PlayerId String Bool -- the string is the join code and the bool is whether to show the "join code was wrong" error message
    | ConnectingToGame PlayerId
    | NamingPlayer PlayerId String Lobby
    | EnteringLobby PlayerId Lobby
    | InGame PlayerId Lobby


type alias FrontendModel =
    { key : Key -- used by Browser.Navigation for things like pushUrl
    , state : FrontendState
    }


type alias BackendModel =
    { lobbies : Dict LobbyId Lobby
    , seed : Random.Seed
    , playerIdMap : Dict Lamdera.ClientId PlayerId
    , playerIdNonce : PlayerId
    , lobbyIdNonce : LobbyId -- the id that will be assigned to the next created lobby
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
    | CreateLobby
    | JoinGame String
    | NamePlayer LobbyId String


type BackendMsg
    = NoOpBackendMsg
    | HandleConnect Lamdera.SessionId Lamdera.ClientId
    | HandleDisconnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | AssignPlayerId PlayerId
    | UpdateLobby Lobby
    | JoinGameFailed
