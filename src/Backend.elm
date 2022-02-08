module Backend exposing (..)

import Dict
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
      , players = Dict.empty
      , gameIdNonce = 0
      , playerIdNonce = 0
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        HandleConnect _ clientId ->
            ( { model
                | players =
                    model.players
                        |> Dict.insert clientId (Player model.playerIdNonce Nothing)
                , playerIdNonce = model.playerIdNonce + 1
              }
            , Cmd.none
            )

        HandleDisconnect _ clientId ->
            ( { model
                | players =
                    model.players
                        |> Dict.remove clientId
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOpToBackend ->
            noOp

        CreateGame ->
            let
                gameState =
                    GameState model.gameIdNonce "JKLM" [ clientId ]
            in
            ( { model
                | games =
                    model.games
                        |> Dict.insert model.gameIdNonce gameState
                , gameIdNonce = model.gameIdNonce + 1
              }
            , Lamdera.sendToFrontend clientId (GameJoined gameState)
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
                            ( { model
                                | games = Dict.insert gameId { game | players = clientId :: game.players } model.games
                              }
                            , Lamdera.sendToFrontend clientId (GameJoined game)
                            )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect HandleConnect
        , Lamdera.onDisconnect HandleDisconnect
        ]
