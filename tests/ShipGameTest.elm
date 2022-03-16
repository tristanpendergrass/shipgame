module ShipGameTest exposing (..)

import Expect
import List.Nonempty exposing (Nonempty(..))
import Player exposing (PlayerId)
import Random
import ShipGame exposing (ShipGame)
import Test exposing (Test, describe, test)


defaultPlayers : Nonempty PlayerId
defaultPlayers =
    Nonempty 1 [ 2, 3, 4 ]


defaultShipGame : ShipGame
defaultShipGame =
    ShipGame.create defaultPlayers (Random.initialSeed 0)


suite : Test
suite =
    describe "ShipGame module"
        [ describe "getPlayers"
            [ test "gets players" <|
                \_ ->
                    Expect.equal
                        (ShipGame.getPlayers defaultShipGame)
                        defaultPlayers
            ]
        ]
