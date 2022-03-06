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


getSelected : SelectionList a -> a
getSelected (SelectionList _ selected _) =
    selected


selectNext : SelectionList a -> Maybe (SelectionList a)
selectNext selectionList =
    case toTuple selectionList of
        ( before, selected, [] ) ->
            Nothing

        ( before, selected, first :: rest ) ->
            Just (fromLists (List.concat [ before, [ selected ] ]) first rest)


toList : SelectionList a -> List a
toList (SelectionList listBefore selected listAfter) =
    List.concat [ listBefore, [ selected ], listAfter ]
