module ShipGameTest exposing (..)

import Dice exposing (Dice(..))
import Expect exposing (Expectation)
import List.Nonempty exposing (Nonempty(..))
import Player exposing (PlayerId)
import SelectionList exposing (SelectionList(..))
import ShipGame exposing (Ship(..), ShipGame, ShipGameInfo, ShipGamePlayer, ShipGameUpdateResult)
import Test exposing (Test, describe, test)


defaultShipGame : ShipGame
defaultShipGame =
    ShipGame.create (Nonempty 1 [ 2, 3, 4 ])


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


createPlayer : PlayerId -> ShipGamePlayer
createPlayer playerId =
    { id = playerId, pastShips = [] }


defaultPlayers : SelectionList ShipGamePlayer
defaultPlayers =
    SelectionList.fromLists [] 1 [ 2, 3, 4 ]
        |> SelectionList.map createPlayer


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
        , test "after player leaves the game" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.LeaveGame 2 ]
                    |> expectShipGameInfo
                        { round = 0
                        , players =
                            SelectionList.fromLists [] 1 [ 3, 4 ]
                                |> SelectionList.map createPlayer
                        , dice = NeverRolled
                        }
        , test "after current player leaves the game" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.LeaveGame 1 ]
                    |> expectShipGameInfo
                        { round = 0
                        , players =
                            SelectionList.fromLists [] 2 [ 3, 4 ]
                                |> SelectionList.map createPlayer
                        , dice = NeverRolled
                        }
        , test "after playing a whole turn with a complete ship" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates
                        [ ShipGame.Roll defaultNumbers
                        , ShipGame.Keep 4 -- keep the 6
                        , ShipGame.Keep 3 -- keep the 5
                        , ShipGame.Keep 2 -- keep the 4
                        , ShipGame.Roll { first = 3, second = 4, third = 4, fourth = 5, fifth = 6 }
                        , ShipGame.Keep 1 -- keep the 4
                        , ShipGame.Roll { first = 5, second = 4, third = 4, fourth = 5, fifth = 6 }
                        , ShipGame.Keep 0 -- keep the 5
                        , ShipGame.Pass
                        ]
                    |> expectShipGameInfo
                        { round = 0
                        , players =
                            let
                                player1 =
                                    { id = 1
                                    , pastShips = [ ShipWithFive 4 5 ]
                                    }
                            in
                            SelectionList.fromLists [ player1 ] (createPlayer 2) [ createPlayer 3, createPlayer 4 ]
                        , dice = NeverRolled
                        }
        , test "dice reset after active player leaves" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates
                        [ ShipGame.Roll defaultNumbers
                        , ShipGame.Keep 4 -- keep the 6
                        , ShipGame.Keep 3 -- keep the 5
                        , ShipGame.Keep 2 -- keep the 4
                        , ShipGame.LeaveGame 1 -- player 1 leaves
                        ]
                    |> expectShipGameInfo
                        { round = 0
                        , players =
                            SelectionList.fromLists [] (createPlayer 2) [ createPlayer 3, createPlayer 4 ]
                        , dice = NeverRolled
                        }
        ]
