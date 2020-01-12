module Tests exposing (suite)

import Expect
import List.Unique as UniqueList
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "List.Unique"
        [ describe "toList, fromList"
            [ test "list is the same after toList and fromList" <|
                \() ->
                    Expect.true
                        "(List.Unique.toList <| List.Unique.fromList [ 1, 2, 3 ]) == [ 1, 2, 3 ]"
                        ((UniqueList.toList <| UniqueList.fromList [ 1, 2, 3 ]) == [ 1, 2, 3 ])
            , test "toList removed duplicates" <|
                \() ->
                    Expect.true
                        "(toList <| fromList [ 1, 1, 2, 3 ]) == [ 1, 2, 3 ]"
                        ((UniqueList.toList <| UniqueList.fromList [ 1, 1, 2, 3 ]) == [ 1, 2, 3 ])
            , test "empty toList is an empty list" <|
                \() ->
                    Expect.equal (UniqueList.toList UniqueList.empty) []
            , test "remove 'a' from alphabet" <|
                \() ->
                    Expect.equal
                        ((UniqueList.toList << UniqueList.remove 'a') (UniqueList.fromList [ 'a', 'b', 'c' ]))
                        [ 'b', 'c' ]
            , test "remove 'b' from alphabet" <|
                \() ->
                    Expect.equal
                        ((UniqueList.toList << UniqueList.remove 'b') (UniqueList.fromList [ 'a', 'b', 'c' ]))
                        [ 'a', 'c' ]
            , test "'b' is added after 'a'" <|
                \() ->
                    Expect.equal
                        ((UniqueList.toList << ('b' |> UniqueList.addAfter 'a')) (UniqueList.fromList [ 'a', 'c' ]))
                        [ 'a', 'b', 'c' ]
            , test "'a' is before 'b'" <|
                \() ->
                    Expect.equal (UniqueList.isBefore 'b' 'a' (UniqueList.fromList [ 'a', 'b', 'c' ]))
                        (Just True)
            , test "'z' is before 'b' in abc (should be Nothing)" <|
                \() ->
                    Expect.equal (UniqueList.isBefore 'b' 'z' (UniqueList.fromList [ 'a', 'b', 'c' ]))
                        Nothing
            , test "filterDuplicates [ True, True ] is [ True ]" <|
                \() ->
                    Expect.equal (UniqueList.filterDuplicates [ True, True ]) [ True ]
            ]
        ]
