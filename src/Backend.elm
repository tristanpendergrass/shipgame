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

        OnConnect sessionId clientId ->
            ( model
            , Lamdera.sendToFrontend clientId Greeting
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
    Lamdera.onConnect OnConnect
