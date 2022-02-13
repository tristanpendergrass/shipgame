module Backend exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Html
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
      , seed = Random.initialSeed 0
      , clientIdToPlayerId = Dict.empty
      , playerIdNonce = 0
      , lobbyIdNonce = 0
      }
    , Cmd.none
    )


removePlayerFromLobbies : PlayerId -> Dict LobbyId Lobby -> Dict LobbyId Lobby
removePlayerFromLobbies removedClientId =
    Dict.map
        (\_ lobby ->
            { lobby
                | waitingRoom =
                    lobby.waitingRoom
                        |> List.filter ((/=) removedClientId)
                , game = ShipGame.removePlayer removedClientId lobby.game
            }
        )


removeEmptyGames : Dict LobbyId Lobby -> Dict LobbyId Lobby
removeEmptyGames =
    Dict.filter
        (\_ lobby ->
            let
                noPlayers =
                    lobby.game
                        |> ShipGame.getPlayers
                        |> List.isEmpty

                noWaitingPlayers =
                    List.isEmpty lobby.waitingRoom
            in
            not noPlayers || not noWaitingPlayers
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
                                |> removePlayerFromLobbies playerId
                                |> removeEmptyGames
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
                    List.concat
                        [ ShipGame.getPlayers newLobby.game
                        , newLobby.waitingRoom
                        ]

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
                        ( joinCode, newSeed ) =
                            Random.step generateJoinCode model.seed

                        lobby : Lobby
                        lobby =
                            { id = model.lobbyIdNonce
                            , joinCode = joinCode
                            , waitingRoom = [ playerId ]
                            , playerData = Dict.empty
                            , game = ShipGame.create
                            }
                    in
                    ( { model
                        | lobbies =
                            model.lobbies
                                |> Dict.insert model.lobbyIdNonce lobby
                        , lobbyIdNonce = model.lobbyIdNonce + 1
                        , seed = newSeed
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
                            let
                                maybeLobby =
                                    Dict.get lobbyId model.lobbies
                            in
                            case maybeLobby of
                                Nothing ->
                                    ( model, Lamdera.sendToFrontend clientId JoinGameFailed )

                                Just lobby ->
                                    let
                                        newLobby : Lobby
                                        newLobby =
                                            { lobby
                                                | waitingRoom = playerId :: lobby.waitingRoom
                                            }
                                    in
                                    ( { model
                                        | lobbies =
                                            model.lobbies
                                                |> Dict.insert lobbyId newLobby
                                      }
                                    , sendLobbyUpdateToFrontend newLobby
                                    )

        NamePlayer gameId name ->
            case Dict.get clientId model.clientIdToPlayerId of
                Nothing ->
                    noOp

                Just playerId ->
                    case Dict.get gameId model.lobbies of
                        Nothing ->
                            noOp

                        Just lobby ->
                            let
                                newLobby : Lobby
                                newLobby =
                                    { lobby
                                        | waitingRoom = List.filter ((/=) playerId) lobby.waitingRoom
                                        , game = ShipGame.addPlayer playerId lobby.game
                                        , playerData = Dict.insert playerId (Player playerId (Just name)) lobby.playerData
                                    }
                            in
                            ( { model
                                | lobbies =
                                    model.lobbies
                                        |> Dict.insert gameId newLobby
                              }
                            , sendLobbyUpdateToFrontend newLobby
                            )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect HandleConnect
        , Lamdera.onDisconnect HandleDisconnect
        ]
