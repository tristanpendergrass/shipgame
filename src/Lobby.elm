module Lobby exposing (..)

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)
import Player exposing (Player, PlayerId)
import Random
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
    , gameWrapper : GameWrapper
    }


create : LobbyId -> String -> PlayerId -> Random.Seed -> Lobby
create lobbyId joinCode playerId seed =
    { id = lobbyId
    , joinCode = joinCode
    , gameWrapper = NotStarted [ playerId ]
    }


removePlayer : PlayerId -> Lobby -> Lobby
removePlayer removedPlayerId lobby =
    { lobby
        | gameWrapper =
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


type StartLobbyErr
    = NotEnoughPlayers
    | GameAlreadyStarted


startGame : Lobby -> Result StartLobbyErr Lobby
startGame lobby =
    case lobby.gameWrapper of
        NotStarted (firstPlayer :: rest) ->
            let
                game =
                    ShipGame.create (List.Nonempty.Nonempty firstPlayer rest)
            in
            Ok { lobby | gameWrapper = InProgress game }

        NotStarted [] ->
            Err NotEnoughPlayers

        _ ->
            Err GameAlreadyStarted


endGame : Lobby -> Lobby
endGame lobby =
    case lobby.gameWrapper of
        InProgress game ->
            { lobby | gameWrapper = NotStarted <| List.Nonempty.toList (ShipGame.getPlayers game) }

        _ ->
            lobby


addPlayer : PlayerId -> Lobby -> Maybe Lobby
addPlayer playerId lobby =
    case lobby.gameWrapper of
        NotStarted playerIds ->
            Just { lobby | gameWrapper = NotStarted (playerId :: playerIds) }

        InProgress _ ->
            Nothing


updateGame : ShipGame.ShipGameMsg -> Lobby -> Lobby
updateGame shipGameMsg lobby =
    case lobby.gameWrapper of
        NotStarted _ ->
            lobby

        InProgress game ->
            case ShipGame.update shipGameMsg game of
                ShipGame.GameOver _ ->
                    -- TODO: handle game over
                    { lobby | gameWrapper = NotStarted [] }

                ShipGame.GameContinues newGame ->
                    { lobby | gameWrapper = InProgress newGame }
