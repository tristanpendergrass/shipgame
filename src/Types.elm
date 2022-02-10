module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera
import Random
import Set exposing (Set)
import Url exposing (Url)


type alias PlayerId =
    Int


type alias GameId =
    Int


type alias Player =
    { id : PlayerId
    , displayName : Maybe String
    }


type alias GameState =
    { id : GameId
    , joinCode : String -- the code that players can use to join the game
    , players : Dict PlayerId Player
    , unnamedPlayers : Dict PlayerId Player
    }


type FrontendState
    = Unconnected
    | MainMenu PlayerId String Bool -- the string is the join code and the bool is whether to show the "join code was wrong" error message
    | ConnectingToGame PlayerId
    | NamingPlayer PlayerId String GameState
    | EnteringLobby PlayerId GameState
    | InGame PlayerId GameState


type alias FrontendModel =
    { key : Key -- used by Browser.Navigation for things like pushUrl
    , state : FrontendState
    }


type alias BackendModel =
    { games : Dict GameId GameState
    , seed : Random.Seed
    , playerIdMap : Dict Lamdera.ClientId PlayerId
    , playerIdNonce : PlayerId
    , gameIdNonce : GameId -- the id that will be assigned to the next created game
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | HandleJoinCodeInput String
    | HandleJoinCodeSubmit
    | HandleCreateGameButtonClick
    | HandleNameInput String
    | HandleNameSubmit


type ToBackend
    = NoOpToBackend
    | CreateGame
    | JoinGame String
    | NamePlayer GameId String


type BackendMsg
    = NoOpBackendMsg
    | HandleConnect Lamdera.SessionId Lamdera.ClientId
    | HandleDisconnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | AssignPlayerId PlayerId
    | UpdateGame GameState
    | JoinGameFailed
