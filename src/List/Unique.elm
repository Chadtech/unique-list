module List.Unique exposing
    ( UniqueList
    , fromList, toList, empty
    , length, reverse, member
    , cons, addBefore, addAfter
    , remove, isEmpty, isBefore, isAfter, isFirst
    , filterDuplicates
    )

{-| An ordered list that contains unique elements.


# UniqueList

@docs UniqueList


# Create

@docs fromList, toList, empty


# Utilities

@docs length, reverse, member


# Combine

@docs cons, addBefore, addAfter


# Deconstruct

@docs remove, isEmpty, isBefore, isAfter, isFirst


# Util

@docs filterDuplicates

-}


{-| A list that has no duplicate elements, much like a `Set`.
However, `UniqueList` preserves the initial order of elements and
does not require that elements be `comparable`.
-}
type UniqueList a
    = UniqueList (List a)


{-| Create a `UniqueList` from a `List`.

**Note:** Elements are placed in the position at
which they occurred last.

    fromList [ 3, 1, 2, 3 ] == fromList [ 1, 2, 3 ]

-}
fromList : List a -> UniqueList a
fromList list =
    UniqueList (List.foldr consIfNotMember [] list)


{-| Turn a `UniqueList` into a `List`.

    [ 1 ] == (toList << fromList) [ 1, 1 ]

-}
toList : UniqueList a -> List a
toList (UniqueList list) =
    list


{-| Create an empty `UniqueList`.

    empty == fromList []

-}
empty : UniqueList a
empty =
    UniqueList []


{-| Check if a `UniqueList` is empty.

    isEmpty (fromList []) == True

    isEmpty (fromList [ 1 ]) == False

-}
isEmpty : UniqueList a -> Bool
isEmpty (UniqueList list) =
    List.isEmpty list


{-| Determine the number of elements in a `UniqueList`.

    -- Without duplicates
    length (fromList [ 1, 2, 3 ]) == 3

    -- With duplicates
    length (fromList [ 2, 2, 2 ]) == 1

-}
length : UniqueList a -> Int
length (UniqueList list) =
    List.length list


{-| Reverse a `UniqueList`.

    fromList [ "first", "second" ] == reverse (fromList [ "second", "first" ])

    fromList [ 1, 2, 3, 3 ] == reverse (fromList [ 3, 2, 1 ])

-}
reverse : UniqueList a -> UniqueList a
reverse (UniqueList list) =
    UniqueList (List.reverse list)


{-| Determine if a `UniqueList` contains a value.

    member 4 (fromList [ 1, 4, 6 ]) == True

    member "cat" (fromList [ "dog", "bird" ]) == False

-}
member : a -> UniqueList a -> Bool
member el (UniqueList list) =
    List.member el list


{-| Remove a value from a `UniqueList` if the value is present.

    remove 2 (fromList [ 1, 2, 3 ]) == fromList [ 1, 3 ]

    remove 0 (fromList [ 1, 2, 3 ]) == fromList [ 1, 2, 3 ]

-}
remove : a -> UniqueList a -> UniqueList a
remove element (UniqueList list) =
    UniqueList (filterFor element list)


{-| Add an element to the front of a `UniqueList`.

**Note:** If the element was already in the list, it will be moved to the
front of the list.

    -- Add an element
    cons 1 (fromList [ 2, 3, 4 ]) == fromList [ 1, 2, 3, 4 ]

    -- Move an element
    cons 3 (fromList [ 1, 2, 3 ]) == fromList [ 3, 1, 2 ]

-}
cons : a -> UniqueList a -> UniqueList a
cons element (UniqueList list) =
    UniqueList (element :: filterFor element list)


{-| Add an element to a `UniqueList` before another element.

**Note:** If the added element is already in the list, it will be moved to the
new position.

    -- Add an element
    addBefore 2 6 (fromList [ 0, 2, 1 ]) == fromList [ 0, 6, 2, 1 ]

    -- Move an element
    addBefore 4 1 (fromList [ 1, 2, 3, 4 ]) == fromList [ 2, 3, 1, 4 ]

    -- No effect
    addBefore 0 1 (fromList [ 1, 2 ]) == fromList [ 1, 2 ]

-}
addBefore : a -> a -> UniqueList a -> UniqueList a
addBefore el newEl (UniqueList list) =
    if newEl /= el then
        list
            |> List.foldr (addBeforeHelper el newEl) []
            |> fromList

    else
        UniqueList list


addBeforeHelper : a -> a -> a -> List a -> List a
addBeforeHelper el newEl thisEl newList =
    if thisEl == el then
        newEl :: thisEl :: newList

    else
        thisEl :: newList


{-| Add an element to a `UniqueList` after another element

**Note:** If the added element is already in the list, it will be moved to the
new position.

    -- Add an element
    addAfter 2 3 (fromList [ 1, 2, 4, 5 ]) == fromList [ 1, 2, 3, 4, 5 ]

    -- Move an element
    addAfter 4 1 (fromList [ 1, 2, 3, 4 ]) == fromList [ 2, 3, 4, 1 ]

    -- No effect
    addAfter 0 1 (fromList [ 1, 2 ]) == fromList [ 1, 2 ]

-}
addAfter : a -> a -> UniqueList a -> UniqueList a
addAfter el newEl (UniqueList list) =
    if newEl /= el then
        list
            |> List.foldr (addAfterHelper el newEl) []
            |> fromList

    else
        UniqueList list


addAfterHelper : a -> a -> a -> List a -> List a
addAfterHelper el newEl thisEl newList =
    if thisEl == el then
        el :: newEl :: newList

    else
        thisEl :: newList


{-| Check if an element is before another in a `UniqueList`.

Returns `Nothing` if either of the elements being queried are not in the list.

    germanStates = fromList [ "Bavaria", "Brandenberg" ]

    ("Bavaria" |> isBefore "Brandenberg") germanStates == Just True

    ("Brandenburg" |> isBefore "Bavaria") germanStates == Just False

    ("Bavaria" |> isBefore "New York City") germanStates == Nothing

-}
isBefore : a -> a -> UniqueList a -> Maybe Bool
isBefore after first (UniqueList list) =
    case ( getOrderHelper after list, getOrderHelper first list ) of
        ( Just afterIndex, Just firstIndex ) ->
            if firstIndex < afterIndex then
                Just True

            else
                Just False

        _ ->
            Nothing


{-| Check if an element is after another in a `UniqueList`.

Returns `Nothing` if either of the elements being queried are not in the list.

-}
isAfter : a -> a -> UniqueList a -> Maybe Bool
isAfter first after order =
    isBefore after first order


{-| Check if an element is the first in a `UniqueList`.

Returns `Nothing` if the list is empty.

-}
isFirst : a -> UniqueList a -> Maybe Bool
isFirst el (UniqueList list) =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x == el then
                Just True

            else
                Just False


{-| Remove duplicates from a list without using the `UniqueList` type.

**Note:** Elements are placed in the position at
which they occurred last.

Similar to the `unique` function in `elm-community/list-extra`,
however `List.Extra.unique` only works on `List comparable`.

    filterDuplicates [ True, True ] == [ True ]

    filterDuplicates [ 1, 1, 2, 3, 5 ] == [ 1, 2, 3, 5 ]

    filterDuplicates [ 1, 2, 3, 4, 1 ] == [ 2, 3, 4, 1 ]

-}
filterDuplicates : List a -> List a
filterDuplicates =
    fromList >> toList



-- HELPERS --


consIfNotMember : a -> List a -> List a
consIfNotMember el list =
    if List.member el list then
        list

    else
        el :: list


getOrderHelper : a -> List a -> Maybe Int
getOrderHelper el list =
    getOrderRecursive el ( List.indexedMap Tuple.pair list, Nothing )
        |> Tuple.second


getOrderRecursive : a -> ( List ( Int, a ), Maybe Int ) -> ( List ( Int, a ), Maybe Int )
getOrderRecursive el ( list, maybeIndex ) =
    case list of
        ( index, xEl ) :: xs ->
            if xEl == el then
                ( [], Just index )

            else
                getOrderRecursive el ( xs, maybeIndex )

        [] ->
            ( [], Nothing )


filterFor : a -> List a -> List a
filterFor element list =
    List.filter ((/=) element) list
