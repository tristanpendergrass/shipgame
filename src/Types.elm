module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera
import Lobby exposing (Lobby, LobbyId)
import Player exposing (PlayerId)
import Random
import Sessions exposing (Sessions)
import ShipGame
import Url exposing (Url)


type FrontendState
    = Unconnected
    | MainMenu PlayerId String Bool -- the string is the join code and the bool is whether to show the "join code was wrong" error message
    | ConnectingToGame PlayerId
    | NamingPlayer PlayerId String Lobby
    | ConfirmingName PlayerId Lobby
    | InGame PlayerId Lobby


type alias FrontendModel =
    { key : Key -- used by Browser.Navigation for things like pushUrl
    , state : FrontendState
    }


type alias BackendModel =
    { lobbies : Dict LobbyId Lobby
    , seed : Random.Seed
    , playerIdNonce : PlayerId
    , lobbyIdNonce : LobbyId -- the id that will be assigned to the next created lobby
    , sessions : Sessions
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
    | HandleStartGameClick
    | HandleEndGameClick
    | HandleRoll


type ToBackend
    = NoOpToBackend
    | CreateLobby
    | JoinGame String
    | NamePlayer LobbyId String
    | StartGame LobbyId
    | EndGame LobbyId
    | UpdateGame LobbyId ShipGame.ShipGameMsg
    | UpdateGameWithRoll LobbyId


type BackendMsg
    = NoOpBackendMsg
    | HandleConnect Lamdera.SessionId Lamdera.ClientId
    | HandleDisconnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | AssignPlayerId PlayerId
    | UpdateLobby Lobby
    | JoinGameFailed
