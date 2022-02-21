module ShipGame exposing
    ( ShipGame
    , create
    , getPlayers
    , removePlayer
    )

import Dict exposing (Dict)
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


updateDice : Dice -> ShipGame -> ShipGame
updateDice newDice shipGame =
    { shipGame | dice = newDice }


updateSeed : Random.Seed -> ShipGame -> ShipGame
updateSeed newSeed shipGame =
    { shipGame | seed = newSeed }


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
                            List.length diceValues

                        ( newDiceValues, newSeed ) =
                            Random.step (rollDice numDiceToRoll) shipGame.seed

                        newDice =
                            RolledThrice newDiceValues

                        newGame =
                            shipGame
                                |> updateDice newDice
                                |> updateSeed newSeed
                    in
                    GameContinues newGame

                RolledOnce diceValues ->
                    let
                        numDiceToRoll =
                            List.length diceValues

                        ( newDiceValues, newSeed ) =
                            Random.step (rollDice numDiceToRoll) shipGame.seed

                        newDice =
                            RolledTwice newDiceValues

                        newShipGame =
                            shipGame
                                |> updateDice newDice
                                |> updateSeed newSeed
                    in
                    GameContinues newShipGame

                NeverRolled ->
                    let
                        numDiceToRoll =
                            5

                        ( newDiceValues, newSeed ) =
                            Random.step (rollDice numDiceToRoll) shipGame.seed

                        newDice =
                            RolledOnce newDiceValues

                        newShipGame =
                            shipGame
                                |> updateDice newDice
                                |> updateSeed newSeed
                    in
                    GameContinues newShipGame

        _ ->
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
