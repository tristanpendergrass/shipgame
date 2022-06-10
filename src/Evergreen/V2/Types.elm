module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V2.Lobby
import Evergreen.V2.Player
import Evergreen.V2.Sessions
import Evergreen.V2.ShipGame
import Lamdera
import Random
import Url


type alias MainMenuState =
    { id : Evergreen.V2.Player.PlayerId
    , joinCode : String
    , joinCodeIsInvalid : Bool
    , formSubmitted : Bool
    }


type alias PlayerData =
    Dict.Dict Evergreen.V2.Player.PlayerId Evergreen.V2.Player.Player


type alias InGameState =
    { id : Evergreen.V2.Player.PlayerId
    , lobby : Evergreen.V2.Lobby.Lobby
    , playerData : PlayerData
    , nameInput : String
    }


type FrontendState
    = Unconnected
    | MainMenu MainMenuState
    | InGame InGameState


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , state : FrontendState
    }


type alias BackendModel =
    { lobbies : Dict.Dict Evergreen.V2.Lobby.LobbyId Evergreen.V2.Lobby.Lobby
    , seed : Random.Seed
    , playerIdNonce : Evergreen.V2.Player.PlayerId
    , lobbyIdNonce : Evergreen.V2.Lobby.LobbyId
    , sessions : Evergreen.V2.Sessions.Sessions
    , playerData : PlayerData
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
    | HandleRoll
    | HandlePass
    | HandleKeep Int
    | ReturnToMainMenu


type ToBackend
    = NoOpToBackend
    | CreateLobby
    | JoinGame String
    | NamePlayer String
    | StartGame Evergreen.V2.Lobby.LobbyId
    | EndGame Evergreen.V2.Lobby.LobbyId
    | UpdateGame Evergreen.V2.Lobby.LobbyId Evergreen.V2.ShipGame.ShipGameMsg
    | UpdateGameWithRoll Evergreen.V2.Lobby.LobbyId
    | ExitLobby


type BackendMsg
    = NoOpBackendMsg
    | HandleConnect Lamdera.SessionId Lamdera.ClientId
    | HandleDisconnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | GoToMainMenu Evergreen.V2.Player.PlayerId
    | GoToInGame Evergreen.V2.Player.PlayerId Evergreen.V2.Lobby.Lobby PlayerData
    | UpdateLobby Evergreen.V2.Lobby.Lobby
    | UpdatePlayerData (Dict.Dict Evergreen.V2.Player.PlayerId Evergreen.V2.Player.Player)
    | JoinGameFailed
