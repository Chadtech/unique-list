module Tests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, int, list, tuple)
import List.Unique as UniqueList exposing (UniqueList)
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
            []
        ]


base : Test
base =
    describe "Basic functions"
        [ describe "fromList"
            [ test "list is the same after toList and fromList" <|
                \_ ->
                    Expect.equal
                        (UniqueList.toList <| UniqueList.fromList [ 1, 2, 3 ])
                        [ 1, 2, 3 ]
            ]
        , describe "toList"
            [ test "toList removes duplicates" <|
                \_ ->
                    Expect.equal
                        (UniqueList.toList <| UniqueList.fromList [ 1, 1, 2, 3 ])
                        [ 1, 2, 3 ]
            , test "toList keeps last occurence of duplicate elements" <|
                \_ ->
                    Expect.equal
                        (UniqueList.toList <| UniqueList.fromList [ 1, 5, 2, 3, 4, 5 ])
                        [ 1, 2, 3, 4, 5 ]
            ]
        , describe "empty"
            [ test "empty toList produces an empty list" <|
                \_ ->
                    Expect.equal (UniqueList.toList UniqueList.empty) []
            ]
        ]


unorderedOperations : Test
unorderedOperations =
    let
        sortedElementsAfter f =
            List.sort << applyULFunction f

        removeUsingSets x =
            List.sort << Set.toList << Set.remove x << Set.fromList
    in
    describe "Unordered Operations"
        [ describe "remove"
            [ test "remove 'a' from alphabet" <|
                \_ ->
                    Expect.equal
                        (applyULFunction (UniqueList.remove 'a') [ 'a', 'b', 'c' ])
                        [ 'b', 'c' ]
            , test "remove 'b' from alphabet" <|
                \_ ->
                    Expect.equal
                        (applyULFunction (UniqueList.remove 'b') [ 'a', 'b', 'c' ])
                        [ 'a', 'c' ]
            , fuzz (tuple ( list int, int )) "Removed elements are no longer in the list" <|
                \( xs, target ) ->
                    Expect.equal (sortedElementsAfter (UniqueList.remove target) xs) (removeUsingSets target xs)
            ]
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
        , describe "addAfter"
            [ test "'b' is added after 'a'" <|
                \_ ->
                    Expect.equal
                        (applyULFunction ('b' |> UniqueList.addAfter 'a') [ 'a', 'c' ])
                        [ 'a', 'b', 'c' ]
            ]
        , describe "isFirst" []
        , describe "isBefore"
            [ test "'a' is before 'b'" <|
                \_ ->
                    Expect.equal
                        (UniqueList.fromList [ 'a', 'b', 'c' ] |> ('a' |> UniqueList.isBefore 'b'))
                        (Just True)
            , test "'b' is before 'd' but not immediately" <|
                \_ ->
                    Expect.equal
                        (UniqueList.fromList [ 'a', 'b', 'c', 'd' ] |> ('b' |> UniqueList.isBefore 'd'))
                        (Just True)
            , test "'z' is before 'b' in abc (should be Nothing)" <|
                \_ ->
                    Expect.equal
                        (UniqueList.fromList [ 'a', 'b', 'c' ] |> ('z' |> UniqueList.isBefore 'b'))
                        Nothing
            ]
        , describe "isAfter" []
        ]


helpers : Test
helpers =
    let
        sortedFilterDuplicates =
            List.sort << UniqueList.filterDuplicates
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



-- Helpers


sortedDedupe : List comparable -> List comparable
sortedDedupe =
    List.sort << Set.toList << Set.fromList


recordFuzzer : Fuzzer { value : Int }
recordFuzzer =
    Fuzz.map (\x -> { value = x }) int


applyULFunction : (UniqueList a -> UniqueList a) -> List a -> List a
applyULFunction f =
    UniqueList.toList << f << UniqueList.fromList
