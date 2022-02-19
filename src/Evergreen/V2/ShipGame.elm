module Evergreen.V2.ShipGame exposing (..)

import Evergreen.V2.Player
import Evergreen.V2.SelectionList


type ShipGame
    = ShipGame (Evergreen.V2.SelectionList.SelectionList Evergreen.V2.Player.PlayerId)
