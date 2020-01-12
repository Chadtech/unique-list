module Tests exposing (suite)

import Expect
import Fuzz exposing (char, float, int, list, string)
import List.Unique as UniqueList
import Set
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    Test.concat
        [ base
        , unorderedOperations
        , orderedOperations
        , helpers
        ]


old : Test
old =
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
            ]
        ]


base : Test
base =
    describe "Basic functions"
        [ describe "fromList" []
        , describe "toList" []
        , describe "empty"
            [ test "empty toList produces an empty list" <|
                \_ ->
                    Expect.equal (UniqueList.toList UniqueList.empty) []
            ]
        ]


unorderedOperations : Test
unorderedOperations =
    describe "Unordered Operations"
        [ describe "remove" []
        , describe "length" []
        , describe "member" []
        , describe "isEmpty" []
        ]


orderedOperations : Test
orderedOperations =
    describe "Ordered operations"
        [ describe "cons" []
        , describe "reverse" []
        , describe "addBefore" []
        , describe "addAfter" []
        , describe "isFirst" []
        , describe "isBefore" []
        , describe "isAfter" []
        ]


helpers : Test
helpers =
    let
        sortedDedupe =
            List.sort << Set.toList << Set.fromList

        sortedFilterDuplicates =
            List.sort << UniqueList.filterDuplicates

        recordFuzzer =
            Fuzz.map (\x -> { value = x }) int
    in
    describe "Helper functions"
        [ describe "filterDuplicates"
            [ test "filterDuplicates [ True, True ] is [ True ]" <|
                \_ ->
                    Expect.equal (UniqueList.filterDuplicates [ True, True ]) [ True ]
            , fuzz (list int) "Filtered list should not contain repeated elements" <|
                \xs ->
                    Expect.equal (sortedDedupe xs) (sortedFilterDuplicates xs)
            , fuzz (list recordFuzzer) "Duplicates can be removed from non-comparable lists" <|
                \records ->
                    Expect.equal
                        (records |> List.map .value |> sortedDedupe)
                        (records |> UniqueList.filterDuplicates |> List.map .value |> List.sort)
            ]
        ]
