module Evergreen.V2.ShipGame exposing (..)

import Evergreen.V2.Dice
import Evergreen.V2.Player
import Evergreen.V2.SelectionList


type Ship
    = ShipWithNothing
    | ShipWithOne
    | ShipWithTwo
    | ShipWithThree
    | ShipWithFour Int
    | ShipWithFive Int Int


type alias ShipGamePlayer =
    { id : Evergreen.V2.Player.PlayerId
    , pastShips : List Ship
    }


type alias ShipGame =
    { round : Int
    , players : Evergreen.V2.SelectionList.SelectionList ShipGamePlayer
    , dice : Evergreen.V2.Dice.Dice
    }


type alias GameSummary =
    List ShipGamePlayer


type ShipGameMsg
    = NoOp
    | Roll Evergreen.V2.Dice.RolledNumbers
    | Keep Int
    | Pass
    | LeaveGame Evergreen.V2.Player.PlayerId
