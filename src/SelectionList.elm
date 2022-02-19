module SelectionList exposing (..)


type SelectionList a
    = SelectionList (List a) a (List a)


fromLists : List a -> a -> List a -> SelectionList a
fromLists =
    SelectionList


toTuple : SelectionList a -> ( List a, a, List a )
toTuple (SelectionList listBefore selected listAfter) =
    ( listBefore, selected, listAfter )


removeSelectionListItem : a -> SelectionList a -> Maybe (SelectionList a)
removeSelectionListItem removedItem list =
    let
        ( listBefore, selected, listAfter ) =
            toTuple list

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
                Just (fromLists filteredListBefore newSelection rest)

            ( newSelection :: rest, [] ) ->
                Just (fromLists (List.reverse rest) newSelection [])

    else
        Just (fromLists filteredListBefore selected filteredListAfter)
