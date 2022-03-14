module Dice exposing (Dice, create, diceValueGenerator, keepDie, roll, sort)

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


create : Dice
create =
    NeverRolled


{-| If this function is called with a Dice that's already been RolledThrice it will just return the dice unchanged.
-}
roll : Dice -> Random.Generator Dice
roll dice =
    let
        applyDice : List Int -> List ( Int, Bool ) -> List ( Int, Bool )
        applyDice rolledNumbers =
            List.indexedMap
                (\index ( value, keep ) ->
                    case List.Extra.getAt index rolledNumbers of
                        Just newValue ->
                            if keep then
                                ( value, keep )

                            else
                                ( newValue, keep )

                        Nothing ->
                            ( value, keep )
                )
    in
    case dice of
        NeverRolled ->
            Random.map (\rolledNumbers -> RolledOnce (List.map (\number -> ( number, False )) rolledNumbers)) diceValueGenerator

        RolledOnce values ->
            Random.map (\rolledNumbers -> RolledTwice (applyDice rolledNumbers values)) diceValueGenerator

        RolledTwice values ->
            Random.map (\rolledNumbers -> RolledThrice (applyDice rolledNumbers values)) diceValueGenerator

        RolledThrice _ ->
            Random.constant dice


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
    = KeepDieError


keepDie : Int -> Dice -> Result KeepDieError ( Int, Dice )
keepDie index dice =
    Debug.todo "Implement"



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
