module Backend exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Random
import Random.Char
import Random.String
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
    ( { games = Dict.empty
      , seed = Random.initialSeed 0
      , playerIdMap = Dict.empty
      , playerIdNonce = 0
      , gameIdNonce = 0
      }
    , Cmd.none
    )


removePlayerFromGames : PlayerId -> Dict GameId GameState -> Dict GameId GameState
removePlayerFromGames removedClientId =
    Dict.map
        (\gameId game ->
            { game
                | players =
                    game.players
                        |> Dict.filter (\clientId _ -> clientId /= removedClientId)
            }
        )


removeEmptyGames : Dict GameId GameState -> Dict GameId GameState
removeEmptyGames =
    Dict.filter
        (\_ game ->
            not <| Dict.isEmpty game.players
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
                        | games =
                            model.games
                                |> removePlayerFromGames playerId
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

        sendGameUpdateToFrontend newGameState =
            let
                playerIds =
                    List.concat
                        [ Dict.keys newGameState.players
                        , Dict.keys newGameState.unnamedPlayers
                        ]

                clientIds =
                    playerIds
                        |> List.filterMap (clientIdForPlayerId model.playerIdMap)
            in
            clientIds
                |> List.map
                    (\id ->
                        Lamdera.sendToFrontend id (UpdateGame newGameState)
                    )
                |> Cmd.batch
    in
    case msg of
        NoOpToBackend ->
            noOp

        CreateGame ->
            case Dict.get clientId model.playerIdMap of
                Nothing ->
                    noOp

                Just playerId ->
                    let
                        ( joinCode, newSeed ) =
                            Random.step generateJoinCode model.seed

                        gameState : GameState
                        gameState =
                            GameState model.gameIdNonce joinCode Dict.empty (Dict.singleton playerId (Player playerId Nothing))
                    in
                    ( { model
                        | games =
                            model.games
                                |> Dict.insert model.gameIdNonce gameState
                        , gameIdNonce = model.gameIdNonce + 1
                        , seed = newSeed
                      }
                    , sendGameUpdateToFrontend gameState
                    )

        JoinGame joinCode ->
            case Dict.get clientId model.playerIdMap of
                Nothing ->
                    noOp

                Just playerId ->
                    let
                        maybeGameId =
                            model.games
                                |> Dict.keys
                                |> List.Extra.find
                                    (\idx ->
                                        case Dict.get idx model.games of
                                            Nothing ->
                                                False

                                            Just game ->
                                                game.joinCode == joinCode
                                    )
                    in
                    case maybeGameId of
                        Nothing ->
                            noOp

                        Just gameId ->
                            let
                                maybeGame =
                                    Dict.get gameId model.games
                            in
                            case maybeGame of
                                Nothing ->
                                    noOp

                                Just game ->
                                    let
                                        newGame : GameState
                                        newGame =
                                            { game
                                                | unnamedPlayers =
                                                    game.unnamedPlayers
                                                        |> Dict.insert playerId (Player playerId Nothing)
                                            }
                                    in
                                    ( { model
                                        | games =
                                            model.games
                                                |> Dict.insert gameId newGame
                                      }
                                    , sendGameUpdateToFrontend newGame
                                    )

        NamePlayer gameId name ->
            case Dict.get clientId model.playerIdMap of
                Nothing ->
                    noOp

                Just playerId ->
                    case Dict.get gameId model.games of
                        Nothing ->
                            noOp

                        Just game ->
                            let
                                newGame : GameState
                                newGame =
                                    { game
                                        | players =
                                            game.players
                                                |> Dict.insert playerId (Player playerId (Just name))
                                        , unnamedPlayers =
                                            game.unnamedPlayers
                                                |> Dict.remove playerId
                                    }
                            in
                            ( { model
                                | games =
                                    model.games
                                        |> Dict.insert gameId newGame
                              }
                            , sendGameUpdateToFrontend newGame
                            )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect HandleConnect
        , Lamdera.onDisconnect HandleDisconnect
        ]
