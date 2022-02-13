module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera
import Lobby exposing (Lobby, LobbyId)
import Player exposing (Player, PlayerId)
import Random
import ShipGame exposing (ShipGame)
import Url exposing (Url)


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
    , clientIdToPlayerId : Dict Lamdera.ClientId PlayerId
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
    | StartGame LobbyId


type BackendMsg
    = NoOpBackendMsg
    | HandleConnect Lamdera.SessionId Lamdera.ClientId
    | HandleDisconnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | AssignPlayerId PlayerId
    | UpdateLobby Lobby
    | JoinGameFailed
