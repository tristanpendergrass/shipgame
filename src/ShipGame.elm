module ShipGame exposing
    ( ShipGame
    , create
    , getPlayers
    , removePlayer
    )

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)
import Player exposing (Player, PlayerId)
import SelectList exposing (SelectList)
import Util exposing (removeSelectListItem)


type ShipGame
    = ShipGame (SelectList PlayerId)


create : Nonempty PlayerId -> ShipGame
create (List.Nonempty.Nonempty first rest) =
    ShipGame (SelectList.fromLists [] first rest)


removePlayer : PlayerId -> ShipGame -> Maybe ShipGame
removePlayer playerId shipGame =
    case shipGame of
        ShipGame playerIds ->
            removeSelectListItem playerId playerIds
                |> Maybe.map ShipGame


getPlayers : ShipGame -> Nonempty PlayerId
getPlayers shipGame =
    case shipGame of
        ShipGame playerIds ->
            case SelectList.toTuple playerIds of
                ( [], selected, last ) ->
                    List.Nonempty.Nonempty selected last

                ( first :: rest, selected, last ) ->
                    List.Nonempty.Nonempty first (List.concat [ rest, selected :: last ])
