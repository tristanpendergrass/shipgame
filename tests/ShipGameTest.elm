module ShipGameTest exposing (..)

import Dice exposing (Dice(..))
import Expect
import List.Nonempty exposing (Nonempty(..))
import Player exposing (PlayerId)
import Random
import SelectionList
import ShipGame exposing (Ship(..), ShipGame)
import Test exposing (Test, describe, test)


playerIds : Nonempty PlayerId
playerIds =
    Nonempty 1 [ 2, 3, 4 ]


defaultSeed : Random.Seed
defaultSeed =
    Random.initialSeed 0


defaultShipGame : ShipGame
defaultShipGame =
    ShipGame.create playerIds defaultSeed


suite : Test
suite =
    describe "ShipGame module"
        [ test "initial State" <|
            \_ ->
                Expect.equal
                    (ShipGame.getInfo defaultShipGame)
                    { round = 0
                    , players =
                        SelectionList.fromLists [] 1 [ 2, 3, 4 ]
                            |> SelectionList.map (\playerId -> { id = playerId, ship = ShipWithNothing, pastShips = [] })
                    , dice = NeverRolled
                    }
        ]
