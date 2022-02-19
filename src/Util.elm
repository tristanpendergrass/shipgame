module Util exposing (..)

import SelectList exposing (SelectList)


{-| This function removes all instances of removedItem from the list and selects the item after, or before if the selected item was the last one in the list
-}
removeSelectListItem : a -> SelectList a -> Maybe (SelectList a)
removeSelectListItem removedItem list =
    let
        ( listBefore, selected, listAfter ) =
            SelectList.toTuple list

        filteredListBefore =
            List.filter ((/=) removedItem) listBefore

        filteredListAfter =
            List.filter ((/=) removedItem) listAfter
    in
    if selected == removedItem then
        case ( List.reverse filteredListBefore, filteredListAfter ) of
            ( [], [] ) ->
                Nothing

            ( _, newSelection :: rest ) ->
                Just (SelectList.fromLists filteredListBefore newSelection rest)

            ( newSelection :: rest, [] ) ->
                Just (SelectList.fromLists (List.reverse rest) newSelection [])

    else
        Just (SelectList.fromLists filteredListBefore selected filteredListAfter)
