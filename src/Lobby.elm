module Lobby exposing (..)

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)
import Player exposing (..)
import ShipGame exposing (..)


type alias LobbyId =
    Int


type alias Lobby =
    { id : LobbyId
    , joinCode : String -- the code that players can use to join the game
    , waitingRoom : List PlayerId -- players that have joined the lobby but don't have a name yet, although this is not enforced by types
    , playerData : Dict PlayerId Player -- possible todo: make this live in BackendModel instead and sync to frontend so player preferences will persist lobby to lobby
    , game : Maybe ShipGame
    }


removePlayer : PlayerId -> Lobby -> Lobby
removePlayer removedPlayerId lobby =
    { lobby
        | waitingRoom = List.filter ((/=) removedPlayerId) lobby.waitingRoom
        , game = lobby.game |> Maybe.andThen (ShipGame.removePlayer removedPlayerId)
    }


isEmpty : Lobby -> Bool
isEmpty lobby =
    let
        noActivePlayers =
            lobby.game == Nothing

        noWaitingPlayers =
            List.isEmpty lobby.waitingRoom
    in
    noActivePlayers && noWaitingPlayers


getPlayerIds : Lobby -> List PlayerId
getPlayerIds lobby =
    let
        playersInWaitingRoom =
            lobby.waitingRoom

        playersInGame =
            case lobby.game of
                Nothing ->
                    []

                Just game ->
                    game
                        |> ShipGame.getPlayers
                        |> List.Nonempty.toList
    in
    List.concat
        [ lobby.waitingRoom
        , playersInGame
        ]
