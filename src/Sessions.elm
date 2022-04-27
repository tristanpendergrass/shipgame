module Sessions exposing (..)

import Dict exposing (Dict)
import Dict.Extra
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


updateClientId : SessionId -> ClientId -> Sessions -> Sessions
updateClientId sessionId clientId =
    Dict.update sessionId (Maybe.map (\session -> { session | clientId = Just clientId }))


clientIdForPlayerId : Sessions -> PlayerId -> Maybe ClientId
clientIdForPlayerId sessions playerId =
    sessions
        |> Dict.Extra.find (\_ session -> session.playerId == playerId)
        |> Maybe.andThen (\( _, session ) -> session.clientId)


{-| Get a session for a session id only if the player is in a Lobby.addPlayer
-}
getSessionAndLobbyId : SessionId -> Sessions -> Maybe ( Session, LobbyId )
getSessionAndLobbyId sessionId sessions =
    case Dict.get sessionId sessions of
        Nothing ->
            Nothing

        Just session ->
            case session.lobbyId of
                Nothing ->
                    Nothing

                Just lobbyId ->
                    Just ( session, lobbyId )
