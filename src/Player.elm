module Player exposing (..)


type alias PlayerId =
    Int


type alias Player =
    { id : PlayerId
    , displayName : Maybe String
    }
