module Evergreen.V2.Player exposing (..)


type alias PlayerId =
    Int


type alias Player =
    { id : PlayerId
    , displayName : Maybe String
    }
