module Sessions exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (..)
import Lobby exposing (LobbyId)
import Player exposing (PlayerId)


type SessionLobbyStatus
    = NotInLobby
    | InLobby LobbyId


type alias Session =
    { playerId : PlayerId
    , clientId : Maybe ClientId
    , lobbyId : Maybe LobbyId
    }


type alias Sessions =
    Dict SessionId Session


create : Sessions
create =
    Dict.empty


addSession : SessionId -> ClientId -> PlayerId -> Sessions -> Sessions
addSession sessionId clientId playerId sessions =
    Debug.todo "Implement"
