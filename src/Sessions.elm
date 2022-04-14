module Sessions exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (..)
import Lobby exposing (LobbyId)
import Player exposing (PlayerId)


type SessionLobbyStatus
    = NotInLobby
    | InLobby LobbyId


{-| | clientId | lobbyId | meaning |
|------------|-----------|--------------------------------------|
| Nothing | Nothing | User disconnected while in main menu |
| Nothing | Just x | User disconnected while in lobby x |
| Just x | Nothing | User is connected and in main menu |
| Just x | Just y | User is connected and in lobby y |
|------------|-----------|--------------------------------------|
-}
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
addSession sessionId clientId playerId =
    Dict.insert sessionId { playerId = playerId, clientId = Just clientId, lobbyId = Nothing }
