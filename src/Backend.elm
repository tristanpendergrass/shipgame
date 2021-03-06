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
      , lobbyIdNonce = 1
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


sendLobbyPlayerData : BackendModel -> Lobby -> List (Cmd BackendMsg)
sendLobbyPlayerData backendModel lobby =
    lobby
        |> Lobby.getPlayerIds
        |> List.filterMap (Sessions.clientIdForPlayerId backendModel.sessions)
        |> List.map
            (\clientIdToUpdate ->
                Lamdera.sendToFrontend clientIdToUpdate (UpdatePlayerData backendModel.playerData)
            )


scopePlayerDataToLobby : Lobby -> PlayerData -> PlayerData
scopePlayerDataToLobby lobby playerData =
    let
        playerIds =
            Lobby.getPlayerIds lobby
    in
    playerData
        |> Dict.filter (\playerId _ -> List.member playerId playerIds)


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
                    , Lamdera.sendToFrontend clientId (GoToMainMenu newPlayerId)
                    )

                Just session ->
                    -- User is reconnecting
                    let
                        maybeLobby =
                            session.lobbyId
                                |> Maybe.andThen (\lobbyId -> Dict.get lobbyId model.lobbies)

                        newModel =
                            model
                                |> updateSessions (Dict.insert sessionId { session | clientId = Just clientId })

                        messageToFrontend =
                            case maybeLobby of
                                Nothing ->
                                    Lamdera.sendToFrontend clientId (GoToMainMenu session.playerId)

                                Just lobby ->
                                    let
                                        playerData =
                                            model.playerData
                                                |> scopePlayerDataToLobby lobby
                                    in
                                    Cmd.batch <|
                                        List.concat
                                            [ sendLobbyPlayerData newModel lobby
                                            , [ Lamdera.sendToFrontend clientId (GoToInGame session.playerId lobby playerData) ]
                                            ]
                    in
                    ( newModel
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

        sendLobbyUpdateToFrontend : Lobby -> List (Cmd BackendMsg)
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

                        playerData =
                            model.playerData
                                |> scopePlayerDataToLobby lobby

                        sessions =
                            Sessions.updateLobbyId sessionId lobby.id model.sessions
                    in
                    ( { model
                        | lobbies =
                            model.lobbies
                                |> Dict.insert lobby.id lobby
                        , lobbyIdNonce = model.lobbyIdNonce + 1
                        , seed = seed4
                        , sessions = sessions
                      }
                    , Cmd.batch
                        (List.concat
                            [ sendLobbyUpdateToFrontend lobby
                            , [ Lamdera.sendToFrontend clientId (GoToInGame playerId lobby playerData) ]
                            ]
                        )
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
                                                String.toLower game.joinCode == String.toLower joinCode
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
                                            let
                                                playerData =
                                                    model.playerData
                                                        |> scopePlayerDataToLobby newLobby
                                            in
                                            ( { model
                                                | lobbies = model.lobbies |> Dict.insert lobbyId newLobby
                                                , sessions = Sessions.updateLobbyId sessionId lobbyId model.sessions
                                              }
                                            , Cmd.batch <|
                                                List.concat
                                                    [ sendLobbyUpdateToFrontend newLobby
                                                    , [ Lamdera.sendToFrontend clientId (GoToInGame playerId lobby playerData) ]
                                                    ]
                                            )

        NamePlayer name ->
            case Sessions.getPlayerIdAndLobbyId sessionId model.sessions of
                Nothing ->
                    noOp

                Just ( playerId, lobbyId ) ->
                    let
                        ( _, _ ) =
                            ( playerId, lobbyId )

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
                                    Cmd.batch <|
                                        List.concat
                                            [ sendLobbyPlayerDataUpdate newPlayerData lobby
                                            , sendLobbyUpdateToFrontend lobby
                                            ]
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
                        Ok newLobbyGenerator ->
                            let
                                ( newLobby, newSeed ) =
                                    Random.step newLobbyGenerator model.seed
                            in
                            ( { model
                                | lobbies = Dict.insert lobbyId newLobby model.lobbies
                                , seed = newSeed
                              }
                            , Cmd.batch <| sendLobbyUpdateToFrontend newLobby
                            )

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
                    ( { model | lobbies = Dict.insert lobbyId newLobby model.lobbies }
                    , Cmd.batch <| sendLobbyUpdateToFrontend newLobby
                    )

        UpdateGame lobbyId shipGameMsg ->
            case Dict.get lobbyId model.lobbies of
                Nothing ->
                    noOp

                Just lobby ->
                    let
                        newLobby =
                            Lobby.updateGame shipGameMsg lobby
                    in
                    ( { model | lobbies = Dict.insert lobbyId newLobby model.lobbies }
                    , Cmd.batch <| sendLobbyUpdateToFrontend newLobby
                    )

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
                            Random.step shipGameMsgGenerator model.seed

                        newLobby =
                            Lobby.updateGame shipGameMsg lobby
                    in
                    ( { model
                        | lobbies = Dict.insert lobbyId newLobby model.lobbies
                        , seed = newSeed
                      }
                    , Cmd.batch <| sendLobbyUpdateToFrontend newLobby
                    )

        ExitLobby ->
            case Sessions.getPlayerIdAndLobbyId sessionId model.sessions of
                Nothing ->
                    noOp

                Just ( playerId, lobbyId ) ->
                    case Dict.get lobbyId model.lobbies of
                        Nothing ->
                            noOp

                        Just lobby ->
                            let
                                newLobby =
                                    Lobby.removePlayer playerId lobby

                                newSessions =
                                    model.sessions
                                        |> Sessions.exitLobby sessionId
                            in
                            ( { model
                                | lobbies = Dict.insert lobbyId newLobby model.lobbies
                                , sessions = newSessions
                              }
                            , Cmd.none
                            )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect HandleConnect
        , Lamdera.onDisconnect HandleDisconnect
        ]
