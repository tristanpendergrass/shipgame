module Player exposing (..)


type alias PlayerId =
    Int


type alias Player =
    -- TODO: convert to type Player = Anonymous | Named
    { id : PlayerId
    , displayName : Maybe String
    }
