module Lobby exposing (..)

import Dict exposing (Dict)
import Player exposing (..)
import ShipGame exposing (..)


type alias LobbyId =
    Int


type alias Lobby =
    { id : LobbyId
    , joinCode : String -- the code that players can use to join the game
    , waitingRoom : List PlayerId -- players that have joined the lobby but don't have a name yet, although this is not enforced by types
    , playerData : Dict PlayerId Player -- possible todo: make this live in BackendModel instead and sync to frontend so player preferences will persist lobby to lobby
    , game : ShipGame
    }


removePlayer : PlayerId -> Lobby -> Lobby
removePlayer removedPlayerId lobby =
    { lobby
        | waitingRoom = List.filter ((/=) removedPlayerId) lobby.waitingRoom
        , game = ShipGame.removePlayer removedPlayerId lobby.game
    }


isEmpty : Lobby -> Bool
isEmpty lobby =
    let
        noPlayers =
            lobby.game
                |> ShipGame.getPlayers
                |> List.isEmpty

        noWaitingPlayers =
            List.isEmpty lobby.waitingRoom
    in
    not noPlayers || not noWaitingPlayers
