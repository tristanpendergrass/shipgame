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


toTupleList : SelectionList a -> List ( a, Bool )
toTupleList selectionList =
    let
        ( listBefore, selected, listAfter ) =
            toTuple selectionList
    in
    List.concat
        [ List.map (\x -> ( x, False )) listBefore
        , [ ( selected, True ) ]
        , List.map (\x -> ( x, False )) listAfter
        ]


mapSelected : (a -> a) -> SelectionList a -> SelectionList a
mapSelected fn (SelectionList listBefore selected listAfter) =
    fromLists listBefore (fn selected) listAfter


map : (a -> b) -> SelectionList a -> SelectionList b
map fn (SelectionList listBefore selected listAfter) =
    fromLists (List.map fn listBefore) (fn selected) (List.map fn listAfter)


selectFirst : SelectionList a -> SelectionList a
selectFirst selectionList =
    case selectionList of
        SelectionList (newSelected :: listBefore) oldSelected listAfter ->
            fromLists [] newSelected (List.concat [ listBefore, [ oldSelected ], listAfter ])

        SelectionList [] _ _ ->
            selectionList
