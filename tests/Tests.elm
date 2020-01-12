module Tests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, int, intRange, list, tuple)
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

        needleAndHaystack =
            tuple ( list (intRange 0 1000), intRange 0 1000 )
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
            , fuzz needleAndHaystack "Removed elements are no longer in the list" <|
                \( haystack, needle ) ->
                    Expect.equal (sortedElementsAfter (UniqueList.remove needle) haystack) (removeUsingSets needle haystack)
            ]
        , describe "length"
            [ test "empty list has length 0" <|
                \_ ->
                    Expect.equal (UniqueList.length UniqueList.empty) 0
            , fuzz (list int) "Length is equal to set cardinality" <|
                \xs ->
                    Expect.equal (UniqueList.length <| UniqueList.fromList xs) (Set.size <| Set.fromList xs)
            ]
        , describe "member"
            [ fuzz needleAndHaystack "Length is equal to set cardinality" <|
                \( haystack, needle ) ->
                    Expect.equal
                        (UniqueList.member needle <| UniqueList.fromList haystack)
                        (Set.member needle <| Set.fromList haystack)
            ]
        , describe "isEmpty"
            [ test "Empty UniqueList is empty" <|
                \_ ->
                    Expect.equal (UniqueList.isEmpty UniqueList.empty) True
            , test "Non-empty list is non-empty" <|
                \_ ->
                    Expect.equal (UniqueList.isEmpty <| UniqueList.fromList [ 1, 2, 3 ]) False
            ]
        ]


orderedOperations : Test
orderedOperations =
    describe "Ordered operations"
        [ describe "cons"
            [ test "Simple cons of 1 to [2,3]" <|
                \_ ->
                    Expect.equal
                        (applyULFunction (UniqueList.cons 1) [ 2, 3 ])
                        [ 1, 2, 3 ]
            , test "Cons moves 1 to the head of  [2,3,1]" <|
                \_ ->
                    Expect.equal
                        (applyULFunction (UniqueList.cons 1) [ 2, 3, 1 ])
                        [ 1, 2, 3 ]
            ]
        , describe "reverse"
            [ test "Reverse of [1,2,3,1] is [1,3,2]" <|
                \_ ->
                    Expect.equal (applyULFunction UniqueList.reverse [ 1, 2, 3, 1 ]) [ 1, 3, 2 ]
            ]
        , describe "addBefore"
            [ test "'b' is added before 'c'" <|
                \_ ->
                    Expect.equal
                        (applyULFunction ('b' |> UniqueList.addBefore 'c') [ 'a', 'c' ])
                        [ 'a', 'b', 'c' ]
            ]
        , describe "addAfter"
            [ test "'b' is added after 'a'" <|
                \_ ->
                    Expect.equal
                        (applyULFunction ('b' |> UniqueList.addAfter 'a') [ 'a', 'c' ])
                        [ 'a', 'b', 'c' ]
            ]
        , describe "isFirst"
            [ test "1 isFirst of [1,2,3]" <|
                \_ ->
                    Expect.equal
                        (UniqueList.isFirst 1 <| UniqueList.fromList [ 1, 2, 3 ])
                        (Just True)
            , test "1 is not first of [2,1,3]" <|
                \_ ->
                    Expect.equal
                        (UniqueList.isFirst 1 <| UniqueList.fromList [ 2, 1, 3 ])
                        (Just False)
            , test "nothing isFirst of []" <|
                \_ ->
                    Expect.equal
                        (UniqueList.isFirst 1 <| UniqueList.fromList [])
                        Nothing
            ]
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
        , describe "isAfter"
            [ test "'c' is after 'a'" <|
                \_ ->
                    Expect.equal
                        (UniqueList.fromList [ 'a', 'b', 'c' ] |> ('c' |> UniqueList.isAfter 'a'))
                        (Just True)
            ]
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
