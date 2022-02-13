module Lobby exposing (..)

import Player exposing (..)
import ShipGame exposing (..)
import Dict exposing (Dict)


type alias LobbyId =
    Int


type alias Lobby =
    { id : LobbyId
    , joinCode : String -- the code that players can use to join the game
    , waitingRoom : List PlayerId -- players that have joined the lobby but don't have a name yet, although this is not enforced by types
    , playerData : Dict PlayerId Player -- possible todo: make this live in BackendModel instead and sync to frontend so player preferences will persist lobby to lobby
    , game : ShipGame
    }
