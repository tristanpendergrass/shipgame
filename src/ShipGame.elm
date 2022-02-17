module ShipGame exposing (..)

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)
import Player exposing (Player, PlayerId)


type ShipGame
    = ShipGameUnstarted (Nonempty PlayerId)
    | ShipGameInProgress (Nonempty PlayerId)
    | ShipGameFinished (Nonempty PlayerId) PlayerId -- <- winner


create : Nonempty PlayerId -> ShipGame
create playerIds =
    ShipGameUnstarted playerIds


start : ShipGame -> ShipGame
start shipGame =
    case shipGame of
        ShipGameUnstarted playerIds ->
            ShipGameInProgress playerIds

        ShipGameInProgress playerIds ->
            ShipGameInProgress playerIds

        ShipGameFinished playerIds winner ->
            ShipGameInProgress playerIds


end : ShipGame -> ShipGame
end shipGame =
    let
        players =
            getPlayers shipGame
    in
    ShipGameFinished players (List.Nonempty.head players)



-- TODO: this should not exist. a ship game shouldn't support adding people in halfway through so it should be created with all players and not support adding


addPlayer : PlayerId -> ShipGame -> ShipGame
addPlayer playerId shipGame =
    case shipGame of
        ShipGameUnstarted playerIds ->
            ShipGameUnstarted (List.Nonempty.append playerIds (List.Nonempty.singleton playerId))

        ShipGameInProgress playerIds ->
            ShipGameInProgress (List.Nonempty.append playerIds (List.Nonempty.singleton playerId))

        ShipGameFinished playerIds winner ->
            ShipGameFinished (List.Nonempty.append playerIds (List.Nonempty.singleton playerId)) winner


removePlayer : PlayerId -> ShipGame -> Maybe ShipGame
removePlayer playerId shipGame =
    let
        playerList =
            getPlayers shipGame
                |> List.Nonempty.toList

        filteredPlayers =
            List.filter ((/=) playerId) playerList
    in
    case filteredPlayers of
        firstPlayer :: rest ->
            case shipGame of
                ShipGameUnstarted _ ->
                    Just <| ShipGameUnstarted (List.Nonempty.Nonempty firstPlayer rest)

                ShipGameInProgress _ ->
                    Just <| ShipGameInProgress (List.Nonempty.Nonempty firstPlayer rest)

                ShipGameFinished _ _ ->
                    Nothing

        [] ->
            Nothing


getPlayers : ShipGame -> Nonempty PlayerId
getPlayers shipGame =
    case shipGame of
        ShipGameUnstarted playerIds ->
            playerIds

        ShipGameInProgress playerIds ->
            playerIds

        ShipGameFinished playerIds _ ->
            playerIds


isStarted : ShipGame -> Bool
isStarted shipGame =
    case shipGame of
        ShipGameUnstarted _ ->
            False

        ShipGameInProgress _ ->
            True

        ShipGameFinished _ _ ->
            True
