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


type ShipGame
    = ShipGame (SelectionList ShipGamePlayer) Int


create : Nonempty PlayerId -> ShipGame
create (List.Nonempty.Nonempty first rest) =
    let
        createPlayer : PlayerId -> ShipGamePlayer
        createPlayer playerId =
            { id = playerId
            , ship = ShipWithNothing
            , score = 0
            }
    in
    ShipGame (SelectionList.fromLists [] (createPlayer first) (List.map createPlayer rest)) 0


removePlayer : PlayerId -> ShipGame -> Maybe ShipGame
removePlayer playerId shipGame =
    case shipGame of
        ShipGame players round ->
            SelectionList.filter (.id >> (/=) playerId) players
                |> Maybe.map (\newPlayers -> ShipGame newPlayers round)


getPlayers : ShipGame -> Nonempty PlayerId
getPlayers shipGame =
    case shipGame of
        ShipGame playerIds _ ->
            case SelectionList.toTuple playerIds of
                ( [], selected, last ) ->
                    List.Nonempty.Nonempty selected last
                        |> List.Nonempty.map .id

                ( first :: rest, selected, last ) ->
                    List.Nonempty.Nonempty first (List.concat [ rest, selected :: last ])
                        |> List.Nonempty.map .id
