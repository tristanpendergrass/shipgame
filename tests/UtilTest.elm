module UtilTest exposing (..)

import Expect
import SelectList exposing (SelectList)
import Test exposing (Test, describe, test)
import Util exposing (..)


suite : Test
suite =
    describe "Util module"
        [ describe "Util.removeSelectListItem"
            [ test "removes an item from middle of before list" <|
                \_ ->
                    Expect.equal
                        (removeSelectListItem 1 (SelectList.fromLists [ 0, 1, 2 ] 3 [ 4, 5, 6 ]))
                        (Just (SelectList.fromLists [ 0, 2 ] 3 [ 4, 5, 6 ]))
            , test "removes an item from middle of after list" <|
                \_ ->
                    Expect.equal
                        (removeSelectListItem 5 (SelectList.fromLists [ 0, 1, 2 ] 3 [ 4, 5, 6 ]))
                        (Just (SelectList.fromLists [ 0, 1, 2 ] 3 [ 4, 6 ]))
            , test "removes multiple items" <|
                \_ ->
                    Expect.equal
                        (removeSelectListItem 1 (SelectList.fromLists [ 0, 1, 2 ] 3 [ 4, 5, 6, 1 ]))
                        (Just (SelectList.fromLists [ 0, 2 ] 3 [ 4, 5, 6 ]))
            , test "removes the selected item" <|
                \_ ->
                    Expect.equal
                        (removeSelectListItem 3 (SelectList.fromLists [ 0, 1, 2 ] 3 [ 4, 5, 6 ]))
                        (Just (SelectList.fromLists [ 0, 1, 2 ] 4 [ 5, 6 ]))
            , test "removes the selected item when it's the last item" <|
                \_ ->
                    Expect.equal
                        (removeSelectListItem 6 (SelectList.fromLists [ 0, 1, 2, 3, 4, 5 ] 6 []))
                        (Just (SelectList.fromLists [ 0, 1, 2, 3, 4 ] 5 []))
            , test "returns Nothing when there's no items left to select" <|
                \_ ->
                    Expect.equal
                        (removeSelectListItem 1 (SelectList.fromLists [ 1, 1, 1 ] 1 [ 1, 1, 1 ]))
                        Nothing
            ]
        ]
