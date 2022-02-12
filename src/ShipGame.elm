module ShipGame exposing (..)

import Dict exposing (Dict)
import Player exposing (Player, PlayerId)


type ShipGame
    = ShipGameUnstarted (Dict PlayerId Player)
    | ShipGameInProgress (Dict PlayerId Player)


namePlayer : PlayerId -> String -> ShipGame -> ShipGame
namePlayer playerId name game =
    case game of
        ShipGameUnstarted players ->
            ShipGameUnstarted (Dict.insert playerId (Player playerId (Just name)) players)

        ShipGameInProgress players ->
            ShipGameInProgress (Dict.insert playerId (Player playerId (Just name)) players)
