module Lobby exposing (..)

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)
import Player exposing (Player, PlayerId)
import Random
import Random.List
import ShipGame exposing (GameSummary, ShipGame)


type alias LobbyId =
    Int


type GameWrapper
    = NotStarted (List PlayerId)
    | InProgress ShipGame
    | Finished GameSummary



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

                Finished players ->
                    let
                        newPlayers : GameSummary
                        newPlayers =
                            players
                                |> List.filter
                                    (\player ->
                                        player.id /= removedPlayerId
                                    )
                    in
                    Finished newPlayers
    }


isEmpty : Lobby -> Bool
isEmpty lobby =
    case lobby.gameWrapper of
        NotStarted playerIds ->
            List.isEmpty playerIds

        InProgress _ ->
            False

        Finished players ->
            List.isEmpty players


getPlayerIds : Lobby -> List PlayerId
getPlayerIds lobby =
    case lobby.gameWrapper of
        NotStarted playerIds ->
            playerIds

        InProgress game ->
            ShipGame.getPlayers game
                |> List.Nonempty.toList

        Finished players ->
            List.map .id players


type StartLobbyErr
    = NotEnoughPlayers
    | GameAlreadyStarted


shuffleNonEmpty : Nonempty a -> Random.Generator (Nonempty a)
shuffleNonEmpty (List.Nonempty.Nonempty first rest) =
    Random.List.shuffle (first :: rest)
        |> Random.map
            (\shuffledList ->
                case shuffledList of
                    [] ->
                        List.Nonempty.Nonempty first rest

                    randomFirst :: randomRest ->
                        List.Nonempty.Nonempty randomFirst randomRest
            )


startGame : Lobby -> Result StartLobbyErr (Random.Generator Lobby)
startGame lobby =
    case lobby.gameWrapper of
        NotStarted [] ->
            Err NotEnoughPlayers

        NotStarted (first :: rest) ->
            Ok <|
                (shuffleNonEmpty (List.Nonempty.Nonempty first rest)
                    |> Random.map
                        (\randomizedPlayerList ->
                            let
                                game =
                                    ShipGame.create randomizedPlayerList
                            in
                            { lobby | gameWrapper = InProgress game }
                        )
                )

        InProgress _ ->
            Err GameAlreadyStarted

        Finished _ ->
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

        Finished _ ->
            Nothing


updateGame : ShipGame.ShipGameMsg -> Lobby -> Lobby
updateGame shipGameMsg lobby =
    case lobby.gameWrapper of
        NotStarted _ ->
            lobby

        InProgress game ->
            case ShipGame.update shipGameMsg game of
                ShipGame.GameOver gameSummary ->
                    { lobby | gameWrapper = Finished gameSummary }

                ShipGame.GameContinues newGame ->
                    { lobby | gameWrapper = InProgress newGame }

        Finished _ ->
            lobby
