module ShipGame exposing (..)

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)
import Player exposing (Player, PlayerId)


type ShipGame
    = ShipGameUnstarted (Nonempty PlayerId)
    | ShipGameInProgress (Nonempty PlayerId)


create : PlayerId -> ShipGame
create playerId =
    ShipGameUnstarted (List.Nonempty.singleton playerId)


addPlayer : PlayerId -> ShipGame -> ShipGame
addPlayer playerId shipGame =
    case shipGame of
        ShipGameUnstarted playerIds ->
            ShipGameUnstarted (List.Nonempty.append playerIds (List.Nonempty.singleton playerId))

        ShipGameInProgress playerIds ->
            ShipGameInProgress (List.Nonempty.append playerIds (List.Nonempty.singleton playerId))


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

        [] ->
            Nothing


getPlayers : ShipGame -> Nonempty PlayerId
getPlayers shipGame =
    case shipGame of
        ShipGameUnstarted playerIds ->
            playerIds

        ShipGameInProgress playerIds ->
            playerIds


isStarted : ShipGame -> Bool
isStarted shipGame =
    case shipGame of
        ShipGameUnstarted _ ->
            False

        ShipGameInProgress _ ->
            True
