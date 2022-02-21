module SelectionList exposing (..)


type SelectionList a
    = SelectionList (List a) a (List a)


fromLists : List a -> a -> List a -> SelectionList a
fromLists =
    SelectionList


toTuple : SelectionList a -> ( List a, a, List a )
toTuple (SelectionList listBefore selected listAfter) =
    ( listBefore, selected, listAfter )


filter : (a -> Bool) -> SelectionList a -> Maybe (SelectionList a)
filter shouldKeep list =
    let
        ( listBefore, selected, listAfter ) =
            toTuple list

        filteredListBefore =
            List.filter shouldKeep listBefore

        filteredListAfter =
            List.filter shouldKeep listAfter
    in
    if shouldKeep selected then
        Just (fromLists filteredListBefore selected filteredListAfter)

    else
        case ( List.reverse filteredListBefore, filteredListAfter ) of
            ( [], [] ) ->
                Nothing

            ( _, newSelection :: rest ) ->
                Just (fromLists filteredListBefore newSelection rest)

            ( newSelection :: rest, [] ) ->
                Just (fromLists (List.reverse rest) newSelection [])
