module ShipGame exposing
    ( ShipGame
    , create
    , getPlayers
    , removePlayer
    )

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)
import Player exposing (Player, PlayerId)
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
    , score : Int
    }


type Dice
    = NeverRolled
      -- The tuple Int is the value rolled. The bool indicates whether the player wants to keep or not. The "keep" value will be ignored by the update fn if it's illegal
    | RolledOnce (List ( Int, Bool )) -- <- The results of the first throw. Should have length 5.
    | RolledTwice (List ( Int, Bool )) -- <- The results of the second throw. Could have length 1 to 5
    | RolledThrice (List ( Int, Bool )) -- <- The results of the third throw. Could have length 1 to 5


type ShipGame
    = ShipGame Int (SelectionList ShipGamePlayer) Dice


create : Nonempty PlayerId -> ShipGame
create (List.Nonempty.Nonempty first rest) =
    let
        round =
            0

        createPlayer : PlayerId -> ShipGamePlayer
        createPlayer playerId =
            { id = playerId
            , ship = ShipWithNothing
            , score = 0
            }

        players =
            SelectionList.fromLists [] (createPlayer first) (List.map createPlayer rest)

        dice =
            NeverRolled
    in
    ShipGame round players dice


removePlayer : PlayerId -> ShipGame -> Maybe ShipGame
removePlayer playerId shipGame =
    case shipGame of
        ShipGame round players dice ->
            let
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
                |> Maybe.map (\newPlayers -> ShipGame round newPlayers newDice)


getPlayers : ShipGame -> Nonempty PlayerId
getPlayers shipGame =
    case shipGame of
        ShipGame _ playerIds _ ->
            case SelectionList.toTuple playerIds of
                ( [], selected, last ) ->
                    List.Nonempty.Nonempty selected last
                        |> List.Nonempty.map .id

                ( first :: rest, selected, last ) ->
                    List.Nonempty.Nonempty first (List.concat [ rest, selected :: last ])
                        |> List.Nonempty.map .id
