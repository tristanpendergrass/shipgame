module ShipGameTest exposing (..)

import Dice exposing (Dice(..))
import Expect exposing (Expectation)
import List.Nonempty exposing (Nonempty(..))
import Player exposing (PlayerId)
import Random
import SelectionList
import ShipGame exposing (Ship(..), ShipGame, ShipGameInfo, ShipGameUpdateResult)
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


expectShipGameInfo : ShipGameInfo -> ShipGame.ShipGameUpdateResult -> Expectation
expectShipGameInfo expectedShipGameInfo shipGameUpdateResult =
    case shipGameUpdateResult of
        ShipGame.GameContinues shipGame ->
            let
                actualShipGameInfo =
                    ShipGame.getInfo shipGame
            in
            Expect.equal expectedShipGameInfo actualShipGameInfo

        ShipGame.GameOver _ ->
            Expect.fail "Game should not be over"


batchShipGameUpdates : List ShipGame.ShipGameMsg -> ShipGame -> ShipGameUpdateResult
batchShipGameUpdates msgs originalShipGame =
    List.foldl
        (\msg shipGameUpdateResult ->
            case shipGameUpdateResult of
                ShipGame.GameContinues shipGame ->
                    ShipGame.update msg shipGame

                ShipGame.GameOver _ ->
                    shipGameUpdateResult
        )
        (ShipGame.GameContinues originalShipGame)
        msgs


suite : Test
suite =
    describe "ShipGame module"
        [ test "initial state" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates []
                    |> expectShipGameInfo
                        { round = 0
                        , players =
                            SelectionList.fromLists [] 1 [ 2, 3, 4 ]
                                |> SelectionList.map (\playerId -> { id = playerId, ship = ShipWithNothing, pastShips = [] })
                        , dice = NeverRolled
                        }
        , test "after rolling once" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.Roll ]
                    |> expectShipGameInfo
                        { round = 0
                        , players =
                            SelectionList.fromLists [] 1 [ 2, 3, 4 ]
                                |> SelectionList.map (\playerId -> { id = playerId, ship = ShipWithNothing, pastShips = [] })
                        , dice = RolledOnce [ ( 4, False ), ( 4, False ), ( 3, False ), ( 3, False ), ( 2, False ) ]
                        }
        , test "after rolling twice" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.Roll, ShipGame.Roll ]
                    |> expectShipGameInfo
                        { round = 0
                        , players =
                            SelectionList.fromLists [] 1 [ 2, 3, 4 ]
                                |> SelectionList.map (\playerId -> { id = playerId, ship = ShipWithNothing, pastShips = [] })
                        , dice = RolledTwice [ ( 2, False ), ( 3, False ), ( 6, False ), ( 6, False ), ( 1, False ) ]
                        }
        , test "after keeping a die" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.Roll, ShipGame.Roll, ShipGame.Keep 2 ]
                    |> expectShipGameInfo
                        { round = 0
                        , players =
                            SelectionList.fromLists [] 1 [ 2, 3, 4 ]
                                |> SelectionList.map (\playerId -> { id = playerId, ship = ShipWithNothing, pastShips = [] })
                        , dice = RolledTwice [ ( 2, False ), ( 3, False ), ( 6, True ), ( 6, False ), ( 1, False ) ]
                        }
        ]
