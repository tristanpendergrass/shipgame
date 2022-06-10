module Evergreen.V2.Lobby exposing (..)

import Evergreen.V2.Player
import Evergreen.V2.ShipGame


type alias LobbyId =
    Int


type GameWrapper
    = NotStarted (List Evergreen.V2.Player.PlayerId)
    | InProgress Evergreen.V2.ShipGame.ShipGame
    | Finished Evergreen.V2.ShipGame.GameSummary


type alias Lobby =
    { id : LobbyId
    , joinCode : String
    , gameWrapper : GameWrapper
    }
