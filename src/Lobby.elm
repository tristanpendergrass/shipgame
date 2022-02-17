module Lobby exposing (..)

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)
import Player exposing (Player, PlayerId)
import ShipGame exposing (ShipGame)


type alias LobbyId =
    Int


type GameWrapper
    = NotStarted (List PlayerId)
    | InProgress ShipGame



-- TODO: Spectators field?


type alias Lobby =
    { id : LobbyId
    , joinCode : String -- the code that players can use to join the game
    , playerData : Dict PlayerId Player -- possible todo: make this live in BackendModel instead and sync to frontend so player preferences will persist lobby to lobby
    , gameWrapper : GameWrapper
    }


create : LobbyId -> String -> PlayerId -> Lobby
create lobbyId joinCode playerId =
    { id = lobbyId
    , joinCode = joinCode
    , playerData = Dict.singleton playerId (Player playerId Nothing)
    , gameWrapper = NotStarted [ playerId ]
    }


removePlayer : PlayerId -> Lobby -> Lobby
removePlayer removedPlayerId lobby =
    { lobby
        | playerData = Dict.remove removedPlayerId lobby.playerData
        , gameWrapper =
            case lobby.gameWrapper of
                NotStarted playerIds ->
                    NotStarted (List.filter ((/=) removedPlayerId) playerIds)

                InProgress game ->
                    case ShipGame.removePlayer removedPlayerId game of
                        Nothing ->
                            NotStarted []

                        Just newGame ->
                            InProgress newGame
    }


isEmpty : Lobby -> Bool
isEmpty lobby =
    case lobby.gameWrapper of
        NotStarted playerIds ->
            List.isEmpty playerIds

        InProgress _ ->
            False


getPlayerIds : Lobby -> List PlayerId
getPlayerIds lobby =
    case lobby.gameWrapper of
        NotStarted playerIds ->
            playerIds

        InProgress game ->
            ShipGame.getPlayers game
                |> List.Nonempty.toList


startGame : Lobby -> Maybe Lobby
startGame lobby =
    case lobby.gameWrapper of
        NotStarted (firstPlayer :: rest) ->
            let
                game =
                    ShipGame.create (List.Nonempty.Nonempty firstPlayer rest)
            in
            Just { lobby | gameWrapper = InProgress game }

        _ ->
            Nothing


endGame : Lobby -> Lobby
endGame lobby =
    case lobby.gameWrapper of
        InProgress game ->
            { lobby | gameWrapper = NotStarted <| List.Nonempty.toList (ShipGame.getPlayers game) }

        _ ->
            lobby


namePlayer : PlayerId -> String -> Lobby -> Lobby
namePlayer playerId name lobby =
    let
        newPlayerData =
            lobby.playerData
                |> Dict.update playerId (Maybe.map (\playerData -> { playerData | displayName = Just name }))
    in
    { lobby | playerData = newPlayerData }


addPlayer : PlayerId -> Lobby -> Maybe Lobby
addPlayer playerId lobby =
    case lobby.gameWrapper of
        NotStarted playerIds ->
            Just
                { lobby
                    | gameWrapper = NotStarted (playerId :: playerIds)
                    , playerData = Dict.insert playerId (Player playerId Nothing) lobby.playerData
                }

        InProgress _ ->
            Nothing
