module SelectionListTest exposing (..)

import Expect
import SelectionList exposing (SelectionList)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "SelectionList module"
        [ describe "removeSelectionListItem"
            [ test "removes an item from middle of before list" <|
                \_ ->
                    Expect.equal
                        (SelectionList.removeSelectionListItem 1 (SelectionList.fromLists [ 0, 1, 2 ] 3 [ 4, 5, 6 ]))
                        (Just (SelectionList.fromLists [ 0, 2 ] 3 [ 4, 5, 6 ]))
            , test "removes an item from middle of after list" <|
                \_ ->
                    Expect.equal
                        (SelectionList.removeSelectionListItem 5 (SelectionList.fromLists [ 0, 1, 2 ] 3 [ 4, 5, 6 ]))
                        (Just (SelectionList.fromLists [ 0, 1, 2 ] 3 [ 4, 6 ]))
            , test "removes multiple items" <|
                \_ ->
                    Expect.equal
                        (SelectionList.removeSelectionListItem 1 (SelectionList.fromLists [ 0, 1, 2 ] 3 [ 4, 5, 6, 1 ]))
                        (Just (SelectionList.fromLists [ 0, 2 ] 3 [ 4, 5, 6 ]))
            , test "removes the selected item" <|
                \_ ->
                    Expect.equal
                        (SelectionList.removeSelectionListItem 3 (SelectionList.fromLists [ 0, 1, 2 ] 3 [ 4, 5, 6 ]))
                        (Just (SelectionList.fromLists [ 0, 1, 2 ] 4 [ 5, 6 ]))
            , test "removes the selected item when it's the last item" <|
                \_ ->
                    Expect.equal
                        (SelectionList.removeSelectionListItem 6 (SelectionList.fromLists [ 0, 1, 2, 3, 4, 5 ] 6 []))
                        (Just (SelectionList.fromLists [ 0, 1, 2, 3, 4 ] 5 []))
            , test "returns Nothing when there's no items left to select" <|
                \_ ->
                    Expect.equal
                        (SelectionList.removeSelectionListItem 1 (SelectionList.fromLists [ 1, 1, 1 ] 1 [ 1, 1, 1 ]))
                        Nothing
            ]
        ]
