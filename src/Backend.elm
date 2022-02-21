module Backend exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Lobby exposing (Lobby, LobbyId)
import Player exposing (Player, PlayerId)
import Random
import Random.Char
import Random.String
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
      , clientIdToPlayerId = Dict.empty
      , playerIdNonce = 0
      , lobbyIdNonce = 0
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        HandleConnect _ clientId ->
            let
                playerId =
                    model.playerIdNonce
            in
            ( { model
                | clientIdToPlayerId =
                    model.clientIdToPlayerId
                        |> Dict.insert clientId playerId
                , playerIdNonce = model.playerIdNonce + 1
              }
            , Lamdera.sendToFrontend clientId (AssignPlayerId playerId)
            )

        HandleDisconnect _ clientId ->
            case Dict.get clientId model.clientIdToPlayerId of
                Just playerId ->
                    ( { model
                        | lobbies =
                            model.lobbies
                                |> Dict.map (\_ lobby -> Lobby.removePlayer playerId lobby)
                                |> Dict.filter (\_ lobby -> Lobby.isEmpty lobby)
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


clientIdForPlayerId : Dict ClientId PlayerId -> PlayerId -> Maybe ClientId
clientIdForPlayerId playerIdMap playerId =
    playerIdMap
        |> Dict.Extra.invert
        |> Dict.get playerId


generateJoinCode : Random.Generator String
generateJoinCode =
    Random.String.string 4 Random.Char.upperCaseLatin


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
                        |> List.filterMap (clientIdForPlayerId model.clientIdToPlayerId)
            in
            clientIds
                |> List.map
                    (\id ->
                        Lamdera.sendToFrontend id (UpdateLobby newLobby)
                    )
                |> Cmd.batch
    in
    case msg of
        NoOpToBackend ->
            noOp

        CreateLobby ->
            case Dict.get clientId model.clientIdToPlayerId of
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
            case Dict.get clientId model.clientIdToPlayerId of
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

        NamePlayer lobbyId name ->
            case Dict.get clientId model.clientIdToPlayerId of
                Nothing ->
                    noOp

                Just playerId ->
                    case Dict.get lobbyId model.lobbies of
                        Nothing ->
                            noOp

                        Just lobby ->
                            let
                                newLobby : Lobby
                                newLobby =
                                    Lobby.namePlayer playerId name lobby
                            in
                            ( { model | lobbies = model.lobbies |> Dict.insert lobbyId newLobby }
                            , sendLobbyUpdateToFrontend newLobby
                            )

        StartGame lobbyId ->
            case Dict.get lobbyId model.lobbies of
                Nothing ->
                    noOp

                Just lobby ->
                    let
                        -- Lobby.startGame will return Nothing if the game was in progress or there are no players in the lobby
                        maybeNewLobby =
                            Lobby.startGame lobby
                    in
                    case maybeNewLobby of
                        Just newLobby ->
                            ( { model | lobbies = Dict.insert lobbyId newLobby model.lobbies }, sendLobbyUpdateToFrontend newLobby )

                        Nothing ->
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


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect HandleConnect
        , Lamdera.onDisconnect HandleDisconnect
        ]
