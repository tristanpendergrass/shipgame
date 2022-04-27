module Backend exposing (..)

import Dice
import Dict exposing (Dict)
import Dict.Extra
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Lobby exposing (Lobby, LobbyId)
import Player exposing (Player, PlayerId)
import Random
import Random.Char
import Random.String
import Sessions exposing (Sessions)
import ShipGame exposing (..)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { lobbies = Dict.empty

      -- TODO: set seed using random process somehow
      , seed = Random.initialSeed 0
      , playerIdNonce = 0
      , lobbyIdNonce = 0
      , sessions = Sessions.create
      , playerData = Dict.empty
      }
    , Cmd.none
    )


updateSessions : (Sessions -> Sessions) -> Model -> Model
updateSessions fn model =
    { model | sessions = fn model.sessions }


updatePlayerIdNonce : (Int -> Int) -> Model -> Model
updatePlayerIdNonce fn model =
    { model | playerIdNonce = fn model.playerIdNonce }


setPlayerData : PlayerData -> Model -> Model
setPlayerData playerData model =
    { model | playerData = playerData }


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOpBackendMsg ->
            noOp

        HandleConnect sessionId clientId ->
            case Dict.get sessionId model.sessions of
                Nothing ->
                    -- User is loading site for the very first time this session
                    let
                        newPlayerId =
                            model.playerIdNonce
                    in
                    ( model
                        |> updateSessions (Sessions.addSession sessionId clientId newPlayerId)
                        |> updatePlayerIdNonce ((+) 1)
                    , Lamdera.sendToFrontend clientId (AssignPlayerId newPlayerId)
                    )

                Just session ->
                    let
                        maybeLobby =
                            session.lobbyId
                                |> Maybe.andThen (\lobbyId -> Dict.get lobbyId model.lobbies)

                        messageToFrontend =
                            case maybeLobby of
                                Nothing ->
                                    Lamdera.sendToFrontend clientId (AssignPlayerId session.playerId)

                                Just lobby ->
                                    -- TODO: notify other players in lobby
                                    Lamdera.sendToFrontend clientId (AssignPlayerIdAndLobby session.playerId lobby)
                    in
                    ( model
                        |> updateSessions (Dict.insert sessionId { session | clientId = Just clientId })
                    , messageToFrontend
                    )

        HandleDisconnect _ _ ->
            noOp


generateJoinCode : Random.Generator String
generateJoinCode =
    Random.String.string 4 Random.Char.upperCaseLatin


giveNameToPlayerId : String -> PlayerId -> PlayerData -> PlayerData
giveNameToPlayerId name playerId =
    Dict.update playerId
        (\maybePlayer ->
            case maybePlayer of
                Nothing ->
                    Just { id = playerId, displayName = Just name }

                Just player ->
                    Just { player | displayName = Just name }
        )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        noOp =
            ( model, Cmd.none )

        sendLobbyUpdateToFrontend : Lobby -> Cmd BackendMsg
        sendLobbyUpdateToFrontend newLobby =
            let
                playerIds =
                    Lobby.getPlayerIds newLobby

                clientIds =
                    playerIds
                        |> List.filterMap (Sessions.clientIdForPlayerId model.sessions)
            in
            clientIds
                |> List.map
                    (\id ->
                        Lamdera.sendToFrontend id (UpdateLobby newLobby)
                    )
                |> Cmd.batch

        sendLobbyPlayerDataUpdate : PlayerData -> Lobby -> List (Cmd BackendMsg)
        sendLobbyPlayerDataUpdate playerData lobby =
            lobby
                |> Lobby.getPlayerIds
                |> List.filterMap (Sessions.clientIdForPlayerId model.sessions)
                |> List.map
                    (\clientIdToUpdate ->
                        Lamdera.sendToFrontend clientIdToUpdate (UpdatePlayerData playerData)
                    )
    in
    case msg of
        NoOpToBackend ->
            noOp

        CreateLobby ->
            let
                maybePlayerId =
                    model.sessions
                        |> Dict.get sessionId
                        |> Maybe.map .playerId
            in
            case maybePlayerId of
                Nothing ->
                    noOp

                Just playerId ->
                    let
                        seed1 =
                            model.seed

                        ( joinCode, seed2 ) =
                            Random.step generateJoinCode seed1

                        ( seed3, seed4 ) =
                            Random.step Random.independentSeed seed2

                        lobby =
                            Lobby.create model.lobbyIdNonce joinCode playerId seed3
                    in
                    ( { model
                        | lobbies =
                            model.lobbies
                                |> Dict.insert lobby.id lobby
                        , lobbyIdNonce = model.lobbyIdNonce + 1
                        , seed = seed4
                      }
                    , sendLobbyUpdateToFrontend lobby
                    )

        JoinGame joinCode ->
            let
                maybePlayerId =
                    model.sessions
                        |> Dict.get sessionId
                        |> Maybe.map .playerId
            in
            case maybePlayerId of
                Nothing ->
                    noOp

                Just playerId ->
                    let
                        maybeLobbyId =
                            model.lobbies
                                |> Dict.keys
                                |> List.Extra.find
                                    (\idx ->
                                        case Dict.get idx model.lobbies of
                                            Nothing ->
                                                False

                                            Just game ->
                                                game.joinCode == joinCode
                                    )
                    in
                    case maybeLobbyId of
                        Nothing ->
                            ( model, Lamdera.sendToFrontend clientId JoinGameFailed )

                        Just lobbyId ->
                            case Dict.get lobbyId model.lobbies of
                                Nothing ->
                                    ( model, Lamdera.sendToFrontend clientId JoinGameFailed )

                                Just lobby ->
                                    case Lobby.addPlayer playerId lobby of
                                        Nothing ->
                                            ( model, Lamdera.sendToFrontend clientId JoinGameFailed )

                                        Just newLobby ->
                                            ( { model | lobbies = model.lobbies |> Dict.insert lobbyId newLobby }
                                            , sendLobbyUpdateToFrontend newLobby
                                            )

        NamePlayer name ->
            -- TODO: clean this function up
            case Sessions.getPlayerIdAndLobbyId sessionId model.sessions of
                Nothing ->
                    noOp

                Just ( playerId, lobbyId ) ->
                    let
                        newPlayerData =
                            model.playerData
                                |> giveNameToPlayerId name playerId

                        maybeLobby =
                            Dict.get lobbyId model.lobbies

                        backendMsg =
                            case maybeLobby of
                                Nothing ->
                                    Cmd.none

                                Just lobby ->
                                    Cmd.batch <| sendLobbyPlayerDataUpdate newPlayerData lobby
                    in
                    ( model
                        |> setPlayerData newPlayerData
                    , backendMsg
                    )

        StartGame lobbyId ->
            case Dict.get lobbyId model.lobbies of
                Nothing ->
                    noOp

                Just lobby ->
                    case Lobby.startGame lobby of
                        Ok newLobby ->
                            ( { model | lobbies = Dict.insert lobbyId newLobby model.lobbies }, sendLobbyUpdateToFrontend newLobby )

                        Err _ ->
                            noOp

        EndGame lobbyId ->
            case Dict.get lobbyId model.lobbies of
                Nothing ->
                    noOp

                Just lobby ->
                    let
                        newLobby =
                            Lobby.endGame lobby
                    in
                    ( { model | lobbies = Dict.insert lobbyId newLobby model.lobbies }, sendLobbyUpdateToFrontend newLobby )

        UpdateGame lobbyId shipGameMsg ->
            case Dict.get lobbyId model.lobbies of
                Nothing ->
                    noOp

                Just lobby ->
                    let
                        newLobby =
                            Lobby.updateGame shipGameMsg lobby
                    in
                    ( { model | lobbies = Dict.insert lobbyId newLobby model.lobbies }, sendLobbyUpdateToFrontend newLobby )

        UpdateGameWithRoll lobbyId ->
            case Dict.get lobbyId model.lobbies of
                Nothing ->
                    noOp

                Just lobby ->
                    let
                        dieGenerator =
                            Random.int 1 6

                        shipGameMsgGenerator =
                            Random.map5 Dice.RolledNumbers dieGenerator dieGenerator dieGenerator dieGenerator dieGenerator
                                |> Random.map ShipGame.Roll

                        ( shipGameMsg, newSeed ) =
                            Random.step shipGameMsgGenerator lobby.seed

                        newLobby =
                            Lobby.updateGame shipGameMsg lobby
                    in
                    ( { model
                        | lobbies = Dict.insert lobbyId newLobby model.lobbies
                        , seed = newSeed
                      }
                    , sendLobbyUpdateToFrontend newLobby
                    )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect HandleConnect
        , Lamdera.onDisconnect HandleDisconnect
        ]
