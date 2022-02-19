module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V2.Lobby
import Evergreen.V2.Player
import Lamdera
import Random
import Url


type FrontendState
    = Unconnected
    | MainMenu Evergreen.V2.Player.PlayerId String Bool
    | ConnectingToGame Evergreen.V2.Player.PlayerId
    | NamingPlayer Evergreen.V2.Player.PlayerId String Evergreen.V2.Lobby.Lobby
    | ConfirmingName Evergreen.V2.Player.PlayerId Evergreen.V2.Lobby.Lobby
    | InGame Evergreen.V2.Player.PlayerId Evergreen.V2.Lobby.Lobby


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , state : FrontendState
    }


type alias BackendModel =
    { lobbies : Dict.Dict Evergreen.V2.Lobby.LobbyId Evergreen.V2.Lobby.Lobby
    , seed : Random.Seed
    , clientIdToPlayerId : Dict.Dict Lamdera.ClientId Evergreen.V2.Player.PlayerId
    , playerIdNonce : Evergreen.V2.Player.PlayerId
    , lobbyIdNonce : Evergreen.V2.Lobby.LobbyId
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | HandleJoinCodeInput String
    | HandleJoinCodeSubmit
    | HandleCreateGameButtonClick
    | HandleNameInput String
    | HandleNameSubmit
    | HandleStartGameClick
    | HandleEndGameClick


type ToBackend
    = NoOpToBackend
    | CreateLobby
    | JoinGame String
    | NamePlayer Evergreen.V2.Lobby.LobbyId String
    | StartGame Evergreen.V2.Lobby.LobbyId
    | EndGame Evergreen.V2.Lobby.LobbyId


type BackendMsg
    = NoOpBackendMsg
    | HandleConnect Lamdera.SessionId Lamdera.ClientId
    | HandleDisconnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | AssignPlayerId Evergreen.V2.Player.PlayerId
    | UpdateLobby Evergreen.V2.Lobby.Lobby
    | JoinGameFailed
