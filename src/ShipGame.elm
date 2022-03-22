module ShipGame exposing
    ( Ship(..)
    , ShipGame
    , ShipGameInfo
    , ShipGameMsg(..)
    , ShipGamePlayer
    , ShipGameUpdateResult(..)
    , create
    , getCurrentPlayer
    , getInfo
    , getPlayers
    , removePlayer
    , update
    )

import Dice exposing (Dice(..))
import Dict exposing (Dict)
import List.Extra
import List.Nonempty exposing (Nonempty)
import Player exposing (Player, PlayerId)
import SelectionList exposing (SelectionList)


type alias ShipGame =
    { round : Int
    , players : SelectionList ShipGamePlayer
    , dice : Dice
    }


type alias ShipGamePlayer =
    { id : PlayerId
    , ship : Ship
    , pastShips : List Ship
    }


type Ship
    = ShipWithNothing
    | ShipWithOne
    | ShipWithTwo
    | ShipWithThree
    | ShipWithFour Int
    | ShipWithFive Int Int


type ShipGameMsg
    = NoOp
    | Roll Dice.RolledNumbers
    | Keep Int -- <- the index of the die to keep. Illegal indexes will be ignored
    | Pass
    | LeaveGame PlayerId


type ShipGameUpdateResult
    = GameContinues ShipGame
    | GameOver (List { id : PlayerId, ships : List Ship })


type alias ShipGameInfo =
    { round : Int
    , players : SelectionList ShipGamePlayer
    , dice : Dice
    }


getInfo : ShipGame -> ShipGameInfo
getInfo shipGame =
    { round = shipGame.round
    , players = shipGame.players
    , dice = shipGame.dice
    }


create : Nonempty PlayerId -> ShipGame
create (List.Nonempty.Nonempty first rest) =
    let
        round =
            0

        createPlayer : PlayerId -> ShipGamePlayer
        createPlayer playerId =
            { id = playerId
            , ship = ShipWithNothing
            , pastShips = []
            }

        players =
            SelectionList.fromLists [] (createPlayer first) (List.map createPlayer rest)

        dice =
            Dice.create
    in
    ShipGame round players dice


getPlayers : ShipGame -> Nonempty PlayerId
getPlayers { players } =
    case SelectionList.toTuple players of
        ( [], selected, last ) ->
            List.Nonempty.Nonempty selected last
                |> List.Nonempty.map .id

        ( first :: rest, selected, last ) ->
            List.Nonempty.Nonempty first (List.concat [ rest, selected :: last ])
                |> List.Nonempty.map .id


removePlayer : PlayerId -> ShipGame -> Maybe ShipGame
removePlayer playerId shipGame =
    let
        { round, players, dice } =
            shipGame

        selectedPlayerWasRemoved =
            players
                |> SelectionList.getSelected
                |> (\player -> player.id == playerId)

        newDice =
            if selectedPlayerWasRemoved then
                Dice.create

            else
                dice
    in
    SelectionList.filter (.id >> (/=) playerId) players
        |> Maybe.map (\newPlayers -> ShipGame round newPlayers newDice)


getCurrentPlayer : ShipGame -> PlayerId
getCurrentPlayer =
    .players >> SelectionList.getSelected >> .id


update : ShipGameMsg -> ShipGame -> ShipGameUpdateResult
update msg shipGame =
    let
        noOp =
            GameContinues shipGame
    in
    case msg of
        NoOp ->
            noOp

        Roll rolledNumbers ->
            GameContinues
                { shipGame
                    | dice = Dice.roll shipGame.dice rolledNumbers
                }

        Pass ->
            let
                playersWithUpdatedSelectedPlayer =
                    SelectionList.mapSelected (\player -> { player | pastShips = player.ship :: player.pastShips, ship = ShipWithNothing }) shipGame.players
            in
            case SelectionList.selectNext playersWithUpdatedSelectedPlayer of
                Just newPlayers ->
                    -- The round is not over, select next player
                    GameContinues { shipGame | dice = Dice.create, players = newPlayers }

                Nothing ->
                    -- All players have passed, round is over.
                    case shipGame.round of
                        2 ->
                            endGame shipGame

                        1 ->
                            GameContinues shipGame

                        0 ->
                            GameContinues shipGame

                        _ ->
                            noOp

        Keep index ->
            case Dice.keepDie index shipGame.dice of
                Err _ ->
                    noOp

                Ok newDice ->
                    let
                        newGame =
                            shipGame
                                |> setDice newDice
                    in
                    GameContinues newGame

        LeaveGame playerId ->
            case SelectionList.filter (.id >> (/=) playerId) shipGame.players of
                Nothing ->
                    GameOver []

                Just newPlayers ->
                    GameContinues { shipGame | players = newPlayers }



-- Internal


type AddToShipError
    = AddToShipError


addToShip : Int -> Ship -> Result AddToShipError Ship
addToShip =
    Debug.todo "Implement"


setDice : Dice -> ShipGame -> ShipGame
setDice newDice shipGame =
    { shipGame | dice = newDice }


updateDice : (Dice -> Dice) -> ShipGame -> ShipGame
updateDice fn shipGame =
    { shipGame | dice = fn shipGame.dice }


updatePlayers : (SelectionList ShipGamePlayer -> SelectionList ShipGamePlayer) -> ShipGame -> ShipGame
updatePlayers fn shipGame =
    { shipGame | players = fn shipGame.players }


updateCurrentShip : (Ship -> Ship) -> ShipGame -> ShipGame
updateCurrentShip fn =
    updatePlayers (SelectionList.mapSelected (\player -> { player | ship = fn player.ship }))


endGame : ShipGame -> ShipGameUpdateResult
endGame shipGame =
    let
        players =
            SelectionList.toList shipGame.players

        convertPlayer player =
            { id = player.id, ships = player.ship :: player.pastShips }

        gameOverPlayerData =
            List.map convertPlayer players
    in
    GameOver gameOverPlayerData


applyDiceToShip : List ( Int, Bool ) -> Ship -> Ship
applyDiceToShip diceValues ship =
    diceValues
        |> List.foldl
            (\( diceValue, keep ) selectedShipResult ->
                Result.andThen
                    (\selectedShip ->
                        if keep then
                            addToShip diceValue selectedShip

                        else
                            Ok selectedShip
                    )
                    selectedShipResult
            )
            (Ok ship)
        |> Result.withDefault ship


addSelectedShipToPastShips : ShipGame -> ShipGame
addSelectedShipToPastShips =
    updatePlayers (SelectionList.mapSelected (\player -> { player | pastShips = player.ship :: player.pastShips, ship = ShipWithNothing }))
