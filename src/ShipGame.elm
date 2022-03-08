module ShipGame exposing
    ( ShipGame
    , create
    , getPlayers
    , removePlayer
    )

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


type Dice
    = NeverRolled
      -- The tuple Int is the value rolled. The bool indicates whether the player wants to keep or not. The "keep" value will be ignored by the update fn if it's illegal
    | RolledOnce (List ( Int, Bool )) -- <- The results of the first throw. Should have length 5.
    | RolledTwice (List ( Int, Bool )) -- <- The results of the second throw. Could have length 1 to 5
    | RolledThrice (List ( Int, Bool )) -- <- The results of the third throw. Could have length 1 to 5


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


rollDice : Int -> Random.Generator (List ( Int, Bool ))
rollDice numDiceToRoll =
    Random.list numDiceToRoll (Random.int 1 6)
        |> Random.map (List.map (\value -> ( value, False )))


setDice : Dice -> ShipGame -> ShipGame
setDice newDice shipGame =
    { shipGame | dice = newDice }


type KeepDieError
    = KeepDieError


setDiceValues : List ( Int, Bool ) -> Dice -> Dice
setDiceValues newDiceValues dice =
    case dice of
        NeverRolled ->
            dice

        RolledOnce _ ->
            RolledOnce newDiceValues

        RolledTwice _ ->
            RolledTwice newDiceValues

        RolledThrice _ ->
            RolledThrice newDiceValues


keepDie : Int -> Dice -> Result KeepDieError ( Int, Dice )
keepDie index dice =
    let
        maybeDiceList =
            case dice of
                NeverRolled ->
                    Nothing

                RolledOnce results ->
                    Just results

                RolledTwice results ->
                    Just results

                RolledThrice results ->
                    Just results
    in
    case maybeDiceList of
        Nothing ->
            Err KeepDieError

        Just diceList ->
            case List.Extra.getAt index diceList of
                Nothing ->
                    Err KeepDieError

                Just ( dieValue, _ ) ->
                    let
                        newDiceList =
                            List.Extra.updateAt index (\( value, _ ) -> ( value, True )) diceList
                    in
                    Ok ( dieValue, setDiceValues newDiceList dice )


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
            case shipGame.dice of
                RolledThrice _ ->
                    -- Can't roll more than three times
                    noOp

                RolledTwice diceValues ->
                    let
                        numDiceToRoll =
                            diceValues
                                |> List.filter (\( _, keep ) -> not keep)
                                |> List.length

                        ( newDiceValues, newSeed ) =
                            Random.step (rollDice numDiceToRoll) shipGame.seed

                        newGame =
                            shipGame
                                |> updateCurrentShip (applyDiceToShip diceValues)
                                |> setDice (RolledThrice newDiceValues)
                                |> setSeed newSeed
                    in
                    GameContinues newGame

                RolledOnce diceValues ->
                    let
                        numDiceToRoll =
                            diceValues
                                |> List.filter (\( _, keep ) -> not keep)
                                |> List.length

                        ( newDiceValues, newSeed ) =
                            Random.step (rollDice numDiceToRoll) shipGame.seed

                        newGame =
                            shipGame
                                |> updateCurrentShip (applyDiceToShip diceValues)
                                |> setDice (RolledTwice newDiceValues)
                                |> setSeed newSeed
                    in
                    GameContinues newGame

                NeverRolled ->
                    let
                        numDiceToRoll =
                            5

                        ( newDiceValues, newSeed ) =
                            Random.step (rollDice numDiceToRoll) shipGame.seed

                        newShipGame =
                            shipGame
                                |> setDice (RolledOnce newDiceValues)
                                |> setSeed newSeed
                    in
                    GameContinues newShipGame

        Pass ->
            let
                playersWithUpdatedSelectedPlayer =
                    SelectionList.mapSelected (\player -> { player | pastShips = player.ship :: player.pastShips, ship = ShipWithNothing }) shipGame.players
            in
            case SelectionList.selectNext playersWithUpdatedSelectedPlayer of
                Just newPlayers ->
                    -- The round is not over, select next player
                    GameContinues { shipGame | dice = NeverRolled, players = newPlayers }

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
            case keepDie index shipGame.dice of
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
            NeverRolled
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
                NeverRolled

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
