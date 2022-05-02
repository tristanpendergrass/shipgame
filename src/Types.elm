module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera
import Lobby exposing (Lobby, LobbyId)
import Player exposing (Player, PlayerId)
import Random
import Sessions exposing (Sessions)
import ShipGame
import Url exposing (Url)


type alias PlayerData =
    Dict PlayerId Player


type FrontendState
    = Unconnected
    | MainMenu { id : PlayerId, joinCode : String, joinCodeIsInvalid : Bool, formSubmitted : Bool }
    | InGame { id : PlayerId, lobby : Lobby, playerData : PlayerData }


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
    , playerData : PlayerData
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
    | NamePlayer String
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
    | GoToMainMenu PlayerId
    | GoToInGame PlayerId Lobby PlayerData
    | UpdateLobby Lobby
    | UpdatePlayerData (Dict PlayerId Player)
    | JoinGameFailed
