module Backend exposing (..)

import Dict exposing (Dict)
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Extra
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
      , playerIdMap = Dict.empty
      , playerIdNonce = 0
      , gameIdNonce = 0
      }
    , Cmd.none
    )


removePlayerFromGames : ClientId -> Dict GameId GameState -> Dict GameId GameState
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
            ( { model
                | games =
                    model.games
                        |> removePlayerFromGames clientId
                        |> removeEmptyGames
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        noOp =
            ( model, Cmd.none )

        sendGameUpdateToFrontend newGameState =
            newGameState.players
                |> Dict.keys
                |> List.map
                    (\player ->
                        Lamdera.sendToFrontend player (UpdateGame newGameState)
                    )
                |> Cmd.batch
    in
    case msg of
        NoOpToBackend ->
            noOp

        CreateGame ->
            let
                gameState : GameState
                gameState =
                    GameState model.gameIdNonce "JKLM" Dict.empty (Dict.singleton clientId (Player clientId Nothing))
            in
            ( { model
                | games =
                    model.games
                        |> Dict.insert model.gameIdNonce gameState
                , gameIdNonce = model.gameIdNonce + 1
              }
            , sendGameUpdateToFrontend gameState
            )

        JoinGame joinCode ->
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
                                                |> Dict.insert clientId (Player clientId Nothing)
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
                                        |> Dict.insert clientId (Player clientId (Just name))
                                , unnamedPlayers =
                                    game.unnamedPlayers
                                        |> Dict.remove clientId
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
