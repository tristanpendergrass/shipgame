module Evergreen.V2.Dice exposing (..)


type Dice
    = NeverRolled
    | RolledOnce (List ( Int, Bool ))
    | RolledTwice (List ( Int, Bool ))
    | RolledThrice (List ( Int, Bool ))


type alias RolledNumbers =
    { first : Int
    , second : Int
    , third : Int
    , fourth : Int
    , fifth : Int
    }
