module Evergreen.V2.Lobby exposing (..)

import Dict
import Evergreen.V2.Player
import Evergreen.V2.ShipGame


type alias LobbyId =
    Int


type GameWrapper
    = NotStarted (List Evergreen.V2.Player.PlayerId)
    | InProgress Evergreen.V2.ShipGame.ShipGame


type alias Lobby =
    { id : LobbyId
    , joinCode : String
    , playerData : Dict.Dict Evergreen.V2.Player.PlayerId Evergreen.V2.Player.Player
    , gameWrapper : GameWrapper
    }
