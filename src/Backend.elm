module Backend exposing (..)

import Dict
import Html
import Lamdera exposing (ClientId, SessionId)
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
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        CreateGame ->
            ( model, Cmd.none )

        JoinGame joinString ->
            ( model, Cmd.none )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect HandleConnect
        , Lamdera.onDisconnect HandleDisconnect
        ]
