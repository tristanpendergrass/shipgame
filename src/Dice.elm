module Dice exposing (Dice(..), RolledNumbers, create, diceValueGenerator, getRolledNumbers, keepDie, roll, sort)

import List.Extra
import Random


{-| Dice is our module for tracking the dice in shipgame. The dice represent your attempts to roll the perfect crew and the current state of your ship.
The state contained in an instance of Dice includes:

  - How many times have these dice been rolled (0, 1, 2, 3)
  - How many of the five dice have been "locked in" and what their values are

```
When a player starts their turn the dice are always in the same NeverRolled state which means there's still five dice available.
After the dice have been rolled once we track how many have been locked in. Technically if there's one, two or three locked in it's unambiguous what their
values are (6 must be locked in first, then 5 then 4) but it would be too complex to incorporate that in the shape of the data so we go with a simple shape
where at all times we track the value and isLocked status for each index 0 through 4. We validate this is correct when told to lock in a die but at no other time.

We also store the dice values in a list even though there's always exactly five so we have access to the list utilities. It should be unlikely we get in trouble
with the wrong number of items in the list.
```

-}
type Dice
    = NeverRolled
      -- The tuple Int is the value rolled. The bool indicates whether the player wants to keep or not. The "keep" value will be ignored by the update fn if it's illegal
    | RolledOnce (List ( Int, Bool )) -- <- The results of the first throw. Should have length 5.
    | RolledTwice (List ( Int, Bool )) -- <- The results of the second throw. Could have length 1 to 5
    | RolledThrice (List ( Int, Bool )) -- <- The results of the third throw. Could have length 1 to 5


type alias RolledNumbers =
    { first : Int
    , second : Int
    , third : Int
    , fourth : Int
    , fifth : Int
    }


create : Dice
create =
    NeverRolled


{-| If this function is called with a Dice that's already been RolledThrice it will just return the dice unchanged.
-}
roll : Dice -> RolledNumbers -> Dice
roll dice { first, second, third, fourth, fifth } =
    let
        applyDice : List Int -> List ( Int, Bool ) -> List ( Int, Bool )
        applyDice numbers =
            List.indexedMap
                (\index ( value, keep ) ->
                    case List.Extra.getAt index numbers of
                        Just newValue ->
                            if keep then
                                ( value, keep )

                            else
                                ( newValue, keep )

                        Nothing ->
                            ( value, keep )
                )

        rolledNumbers =
            [ first, second, third, fourth, fifth ]
    in
    case dice of
        NeverRolled ->
            RolledOnce (List.map (\number -> ( number, False )) rolledNumbers)

        RolledOnce values ->
            RolledTwice (applyDice rolledNumbers values)

        RolledTwice values ->
            RolledThrice (applyDice rolledNumbers values)

        RolledThrice _ ->
            dice


sort : Dice -> Dice
sort =
    let
        compareDieValues : ( Int, Bool ) -> ( Int, Bool ) -> Order
        compareDieValues left right =
            case ( left, right ) of
                ( ( leftValue, True ), ( rightValue, True ) ) ->
                    compare leftValue rightValue

                ( ( _, True ), ( _, False ) ) ->
                    GT

                ( ( _, False ), ( _, True ) ) ->
                    LT

                ( ( leftValue, False ), ( rightValue, False ) ) ->
                    compare leftValue rightValue
    in
    -- This would be called after a player passes to put kept dice at the front of the list
    mapDieValues (List.sortWith compareDieValues)


type KeepDieError
    = KeepDieInvalidIndex
    | KeepDieAlreadyKept
    | KeepDieOutOfOrder -- When user tries to keep e.g. a 5 before a 6 has been kept


keepDie : Int -> Dice -> Result KeepDieError Dice
keepDie index dice =
    let
        maybeValues =
            case dice of
                NeverRolled ->
                    Nothing

                RolledOnce values ->
                    Just values

                RolledTwice values ->
                    Just values

                RolledThrice values ->
                    Just values
    in
    case maybeValues of
        Nothing ->
            Err KeepDieInvalidIndex

        Just values ->
            case List.Extra.getAt index values of
                Nothing ->
                    Err KeepDieInvalidIndex

                Just ( value, keep ) ->
                    if keep then
                        Err KeepDieAlreadyKept

                    else if not (dieIsLegal value values) then
                        Err KeepDieOutOfOrder

                    else
                        Ok (mapDieValues (\_ -> List.Extra.setAt index ( value, True ) values) dice)


getRolledNumbers : Dice -> List Int
getRolledNumbers dice =
    case dice of
        NeverRolled ->
            []

        RolledOnce dieValues ->
            dieValues
                |> List.filter (\( _, keep ) -> keep)
                |> List.map Tuple.first

        RolledTwice dieValues ->
            dieValues
                |> List.filter (\( _, keep ) -> keep)
                |> List.map Tuple.first

        RolledThrice dieValues ->
            dieValues
                |> List.filter (\( _, keep ) -> keep)
                |> List.map Tuple.first



-- Internal


diceValueGenerator : Random.Generator (List Int)
diceValueGenerator =
    Random.list 5 (Random.int 1 6)


mapDieValues : (List ( Int, Bool ) -> List ( Int, Bool )) -> Dice -> Dice
mapDieValues fn dice =
    case dice of
        NeverRolled ->
            NeverRolled

        RolledOnce values ->
            RolledOnce (fn values)

        RolledTwice values ->
            RolledTwice (fn values)

        RolledThrice values ->
            RolledThrice (fn values)


{-| This function will verify that the given die can be kept. It assumes the dieValue are in a legal state.
-}
dieIsLegal : Int -> List ( Int, Bool ) -> Bool
dieIsLegal dieToKeep dieValues =
    let
        keptDieValues =
            dieValues
                |> List.filter Tuple.second
                |> List.map Tuple.first

        hasSix =
            List.member 6 keptDieValues

        hasFive =
            List.member 5 keptDieValues

        hasFour =
            List.member 4 keptDieValues
    in
    if not hasSix then
        dieToKeep == 6

    else if not hasFive then
        dieToKeep == 5

    else if not hasFour then
        dieToKeep == 4

    else
        True
