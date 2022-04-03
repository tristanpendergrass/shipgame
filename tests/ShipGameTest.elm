module ShipGameTest exposing (..)

import Dice exposing (Dice(..))
import Expect exposing (Expectation)
import List.Nonempty exposing (Nonempty(..))
import Player exposing (PlayerId)
import SelectionList exposing (SelectionList(..))
import ShipGame exposing (..)
import Test exposing (Test, describe, test)


defaultShipGame : ShipGame
defaultShipGame =
    ShipGame.create (Nonempty 1 [ 2, 3, 4 ])


expectShipGameInfo : ShipGameInfo -> ShipGameUpdateResult -> Expectation
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


expectGameOver : GameSummary -> ShipGameUpdateResult -> Expectation
expectGameOver expectedGameSummary shipGameUpdateResult =
    case shipGameUpdateResult of
        GameContinues _ ->
            Expect.fail "Game should be over"

        GameOver gameSummary ->
            Expect.equal expectedGameSummary gameSummary


batchShipGameUpdates : List ShipGameMsg -> ShipGame -> ShipGameUpdateResult
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


rolledNumbersLow : Dice.RolledNumbers
rolledNumbersLow =
    { first = 1, second = 2, third = 4, fourth = 5, fifth = 6 }


rolledNumbersHigh : Dice.RolledNumbers
rolledNumbersHigh =
    { first = 3, second = 4, third = 4, fourth = 5, fifth = 6 }


playLowScoringTurn : List ShipGameMsg
playLowScoringTurn =
    [ ShipGame.Roll rolledNumbersLow
    , ShipGame.Keep 4 -- keep the 6
    , ShipGame.Keep 3 -- keep the 5
    , ShipGame.Keep 2 -- keep the 4
    , ShipGame.Keep 1 -- keep the 2
    , ShipGame.Keep 0 -- keep the 1
    , ShipGame.Pass
    ]


playHighScoringTurn : List ShipGameMsg
playHighScoringTurn =
    [ ShipGame.Roll rolledNumbersHigh
    , ShipGame.Keep 4 -- keep the 6
    , ShipGame.Keep 3 -- keep the 5
    , ShipGame.Keep 2 -- keep the 4
    , ShipGame.Keep 1 -- keep the 4
    , ShipGame.Keep 0 -- keep the 3
    , ShipGame.Pass
    ]


playRound : List ShipGameMsg
playRound =
    List.concat
        [ playLowScoringTurn
        , playLowScoringTurn
        , playLowScoringTurn
        , playHighScoringTurn
        ]


lowScoringShip : Ship
lowScoringShip =
    ShipWithFive 1 2


highScoringShip : Ship
highScoringShip =
    ShipWithFive 3 4


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
                    |> batchShipGameUpdates [ ShipGame.Roll rolledNumbersLow ]
                    |> expectShipGameInfo
                        { round = 0
                        , players = defaultPlayers
                        , dice = RolledOnce [ ( 1, False ), ( 2, False ), ( 4, False ), ( 5, False ), ( 6, False ) ]
                        }
        , test "after rolling twice" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.Roll rolledNumbersLow, ShipGame.Roll rolledNumbersHigh ]
                    |> expectShipGameInfo
                        { round = 0
                        , players = defaultPlayers
                        , dice = RolledTwice [ ( 3, False ), ( 4, False ), ( 4, False ), ( 5, False ), ( 6, False ) ]
                        }
        , test "after keeping a die" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.Roll rolledNumbersLow, ShipGame.Keep 4 ]
                    |> expectShipGameInfo
                        { round = 0
                        , players = defaultPlayers
                        , dice = RolledOnce [ ( 1, False ), ( 2, False ), ( 4, False ), ( 5, False ), ( 6, True ) ]
                        }
        , test "after illegally trying to keep a die" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates [ ShipGame.Roll rolledNumbersLow, ShipGame.Keep 1 ]
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
                        [ ShipGame.Roll rolledNumbersLow
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
                        [ ShipGame.Roll rolledNumbersLow
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
        , test "after playing a whole round" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates playRound
                    |> expectShipGameInfo
                        { round = 1
                        , players =
                            SelectionList.fromLists
                                []
                                { id = 1, pastShips = [ lowScoringShip ] }
                                [ { id = 2, pastShips = [ lowScoringShip ] }
                                , { id = 3, pastShips = [ lowScoringShip ] }
                                , { id = 4, pastShips = [ highScoringShip ] }
                                ]
                        , dice = NeverRolled
                        }
        , test "after playing a whole game" <|
            \_ ->
                defaultShipGame
                    |> batchShipGameUpdates
                        (List.concat
                            [ playRound
                            , playRound
                            , playRound
                            ]
                        )
                    |> expectGameOver
                        [ { id = 1, pastShips = [ lowScoringShip, lowScoringShip, lowScoringShip ] }
                        , { id = 2, pastShips = [ lowScoringShip, lowScoringShip, lowScoringShip ] }
                        , { id = 3, pastShips = [ lowScoringShip, lowScoringShip, lowScoringShip ] }
                        , { id = 4, pastShips = [ highScoringShip, highScoringShip, highScoringShip ] }
                        ]
        ]
