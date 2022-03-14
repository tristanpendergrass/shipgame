module ShipGame exposing
    ( ShipGame
    , create
    , getPlayers
    , removePlayer
    )

import Dice exposing (Dice(..))
import Dict exposing (Dict)
import List.Extra
import List.Nonempty exposing (Nonempty)
import Player exposing (Player, PlayerId)
import Random
import SelectionList exposing (SelectionList)


type Ship
    = ShipWithNothing
    | ShipWithOne
    | ShipWithTwo
    | ShipWithThree
    | ShipWithFour Int
    | ShipWithFive Int Int


type AddToShipError
    = AddToShipError


addToShip : Int -> Ship -> Result AddToShipError Ship
addToShip =
    Debug.todo "Implement"


type alias ShipGamePlayer =
    { id : PlayerId
    , ship : Ship
    , pastShips : List Ship
    }


type alias ShipGame =
    { round : Int
    , players : SelectionList ShipGamePlayer
    , dice : Dice
    , seed : Random.Seed
    }


type ShipGameMsg
    = Roll
    | Keep Int -- <- the index of the die to keep. Illegal indexes will be ignored
    | Pass


type ShipGameUpdateResult
    = GameContinues ShipGame
    | GameOver (List { id : PlayerId, ships : List Ship })


setDice : Dice -> ShipGame -> ShipGame
setDice newDice shipGame =
    { shipGame | dice = newDice }


type KeepDieError
    = KeepDieError


updateDice : (Dice -> Dice) -> ShipGame -> ShipGame
updateDice fn shipGame =
    { shipGame | dice = fn shipGame.dice }


setSeed : Random.Seed -> ShipGame -> ShipGame
setSeed newSeed shipGame =
    { shipGame | seed = newSeed }


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


shipGameUpdate : ShipGameMsg -> ShipGame -> ShipGameUpdateResult
shipGameUpdate msg shipGame =
    let
        noOp =
            GameContinues shipGame
    in
    case msg of
        Roll ->
            let
                ( newDice, newSeed ) =
                    Random.step (Dice.roll shipGame.dice) shipGame.seed
            in
            GameContinues
                { shipGame
                    | dice = newDice
                    , seed = newSeed
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
            case Dice.keep index shipGame.dice of
                Err _ ->
                    noOp

                Ok ( dieValue, newDice ) ->
                    let
                        newGame =
                            shipGame
                                |> setDice newDice
                    in
                    Debug.todo "Implement"


create : Nonempty PlayerId -> Random.Seed -> ShipGame
create (List.Nonempty.Nonempty first rest) initialSeed =
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
    ShipGame round players dice initialSeed


removePlayer : PlayerId -> ShipGame -> Maybe ShipGame
removePlayer playerId shipGame =
    let
        { round, players, dice, seed } =
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
        |> Maybe.map (\newPlayers -> ShipGame round newPlayers newDice seed)


getPlayers : ShipGame -> Nonempty PlayerId
getPlayers { players } =
    case SelectionList.toTuple players of
        ( [], selected, last ) ->
            List.Nonempty.Nonempty selected last
                |> List.Nonempty.map .id

        ( first :: rest, selected, last ) ->
            List.Nonempty.Nonempty first (List.concat [ rest, selected :: last ])
                |> List.Nonempty.map .id
