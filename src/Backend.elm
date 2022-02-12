module Backend exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Extra
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
      , playerIdMap = Dict.empty
      , playerIdNonce = 0
      , lobbyIdNonce = 0
      }
    , Cmd.none
    )


getPlayers : Lobby -> Dict PlayerId Player
getPlayers lobby =
    case lobby.game of
        ShipGameUnstarted players ->
            players

        ShipGameInProgress players ->
            players


removePlayerFromLobbies : PlayerId -> Dict LobbyId Lobby -> Dict LobbyId Lobby
removePlayerFromLobbies removedClientId =
    Dict.map
        (\_ lobby ->
            let
                newGame =
                    case lobby.game of
                        ShipGameUnstarted players ->
                            ShipGameUnstarted (Dict.filter (\clientId _ -> clientId /= removedClientId) players)

                        ShipGameInProgress players ->
                            ShipGameInProgress (Dict.filter (\clientId _ -> clientId /= removedClientId) players)
            in
            { lobby | game = newGame }
        )


removeEmptyGames : Dict LobbyId Lobby -> Dict LobbyId Lobby
removeEmptyGames =
    Dict.filter
        (\_ lobby ->
            let
                noPlayers =
                    lobby
                        |> getPlayers
                        |> Dict.isEmpty

                noWaitingPlayers =
                    Dict.isEmpty lobby.waitingRoom
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
                | playerIdMap =
                    model.playerIdMap
                        |> Dict.insert clientId playerId
                , playerIdNonce = model.playerIdNonce + 1
              }
            , Lamdera.sendToFrontend clientId (AssignPlayerId playerId)
            )

        HandleDisconnect _ clientId ->
            case Dict.get clientId model.playerIdMap of
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
                playersToUpdate =
                    getPlayers newLobby

                playerIds =
                    List.concat
                        [ Dict.keys playersToUpdate
                        , Dict.keys newLobby.waitingRoom
                        ]

                clientIds =
                    playerIds
                        |> List.filterMap (clientIdForPlayerId model.playerIdMap)
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
            case Dict.get clientId model.playerIdMap of
                Nothing ->
                    noOp

                Just playerId ->
                    let
                        ( joinCode, newSeed ) =
                            Random.step generateJoinCode model.seed

                        lobby : Lobby
                        lobby =
                            Lobby model.lobbyIdNonce joinCode Dict.empty (ShipGameUnstarted (Dict.singleton playerId (Player playerId Nothing)))
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
            case Dict.get clientId model.playerIdMap of
                Nothing ->
                    noOp

                Just playerId ->
                    let
                        maybeGameId =
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
                    case maybeGameId of
                        Nothing ->
                            ( model, Lamdera.sendToFrontend clientId JoinGameFailed )

                        Just gameId ->
                            let
                                maybeGame =
                                    Dict.get gameId model.lobbies
                            in
                            case maybeGame of
                                Nothing ->
                                    noOp

                                Just game ->
                                    let
                                        newGame : Lobby
                                        newGame =
                                            { game
                                                | waitingRoom =
                                                    game.waitingRoom
                                                        |> Dict.insert playerId (Player playerId Nothing)
                                            }
                                    in
                                    ( { model
                                        | lobbies =
                                            model.lobbies
                                                |> Dict.insert gameId newGame
                                      }
                                    , sendLobbyUpdateToFrontend newGame
                                    )

        NamePlayer gameId name ->
            case Dict.get clientId model.playerIdMap of
                Nothing ->
                    noOp

                Just playerId ->
                    case Dict.get gameId model.lobbies of
                        Nothing ->
                            noOp

                        Just lobby ->
                            let
                                newGame : ShipGame
                                newGame =
                                    ShipGame.namePlayer playerId name lobby.game

                                newLobby : Lobby
                                newLobby =
                                    { lobby
                                        | game = newGame
                                        , waitingRoom =
                                            lobby.waitingRoom
                                                |> Dict.remove playerId
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
