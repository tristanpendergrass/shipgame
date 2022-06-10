module Evergreen.V2.Sessions exposing (..)

import Dict
import Evergreen.V2.Lobby
import Evergreen.V2.Player
import Lamdera


type alias Session =
    { playerId : Evergreen.V2.Player.PlayerId
    , clientId : Maybe Lamdera.ClientId
    , lobbyId : Maybe Evergreen.V2.Lobby.LobbyId
    }


type alias Sessions =
    Dict.Dict Lamdera.SessionId Session
