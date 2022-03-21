module ShipGameTest exposing (..)

import Dice exposing (Dice(..))
import Expect exposing (Expectation)
import List.Nonempty exposing (Nonempty(..))
import Player exposing (PlayerId)
import Random
import SelectionList exposing (SelectionList)
import ShipGame exposing (Ship(..), ShipGame, ShipGameInfo, ShipGamePlayer, ShipGameUpdateResult)
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


defaultPlayers : SelectionList ShipGamePlayer
defaultPlayers =
    SelectionList.fromLists [] 1 [ 2, 3, 4 ]
        |> SelectionList.map (\playerId -> { id = playerId, ship = ShipWithNothing, pastShips = [] })


defaultNumbers : Dice.RolledNumbers
defaultNumbers =
    { first = 1, second = 2, third = 4, fourth = 5, fifth = 6 }


defaultNumbers2 : Dice.RolledNumbers
defaultNumbers2 =
    { first = 3, second = 4, third = 4, fourth = 5, fifth = 6 }


suite : Test
suite =
    describe "ShipGame module"
        [ test "initial state" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates []
                    |> expectShipGameInfo
                        { round = 0
                        , players = defaultPlayers
                        , dice = NeverRolled
                        }
        , test "after rolling once" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.Roll defaultNumbers ]
                    |> expectShipGameInfo
                        { round = 0
                        , players = defaultPlayers
                        , dice = RolledOnce [ ( 1, False ), ( 2, False ), ( 4, False ), ( 5, False ), ( 6, False ) ]
                        }
        , test "after rolling twice" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.Roll defaultNumbers, ShipGame.Roll defaultNumbers2 ]
                    |> expectShipGameInfo
                        { round = 0
                        , players = defaultPlayers
                        , dice = RolledTwice [ ( 3, False ), ( 4, False ), ( 4, False ), ( 5, False ), ( 6, False ) ]
                        }
        , test "after keeping a die" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.Roll defaultNumbers, ShipGame.Keep 4 ]
                    |> expectShipGameInfo
                        { round = 0
                        , players = defaultPlayers
                        , dice = RolledOnce [ ( 1, False ), ( 2, False ), ( 4, False ), ( 5, False ), ( 6, True ) ]
                        }
        , test "after illegally trying to keep a die" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.Roll defaultNumbers, ShipGame.Keep 1 ]
                    |> expectShipGameInfo
                        { round = 0
                        , players = defaultPlayers
                        , dice = RolledOnce [ ( 1, False ), ( 2, False ), ( 4, False ), ( 5, False ), ( 6, False ) ]
                        }
        ]
