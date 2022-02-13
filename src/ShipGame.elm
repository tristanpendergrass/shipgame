module ShipGame exposing (..)

import Dict exposing (Dict)
import Player exposing (Player, PlayerId)


type ShipGame
    = ShipGameUnstarted (List PlayerId)
    | ShipGameInProgress (List PlayerId)



-- TODO: Make this take a player id


create : ShipGame
create =
    ShipGameUnstarted []


addPlayer : PlayerId -> ShipGame -> ShipGame
addPlayer playerId shipGame =
    case shipGame of
        ShipGameUnstarted playerIds ->
            ShipGameInProgress (playerId :: playerIds)

        ShipGameInProgress playerIds ->
            ShipGameInProgress (playerId :: playerIds)


removePlayer : PlayerId -> ShipGame -> ShipGame
removePlayer playerId shipGame =
    case shipGame of
        ShipGameUnstarted playerIds ->
            ShipGameUnstarted (List.filter ((/=) playerId) playerIds)

        ShipGameInProgress playerIds ->
            ShipGameInProgress (List.filter ((/=) playerId) playerIds)


getPlayers : ShipGame -> List PlayerId
getPlayers shipGame =
    case shipGame of
        ShipGameUnstarted playerIds ->
            playerIds

        ShipGameInProgress playerIds ->
            playerIds
