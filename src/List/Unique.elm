module List.Unique exposing
    ( UniqueList
    , empty, isEmpty, fromList, toList, length, reverse, member
    , remove, cons, addBefore, addAfter, isBefore, isAfter, isFirst
    , filterDuplicates
    )

{-| A list that can only contain unique elements


# UniqueList

@docs UniqueList

# Create
@docs fromList, toList, empty

# Utilities

@docs length, reverse, member


# Combine

@docs cons, addBefore, addAfter, isBefore, isAfter, isFirst

# Deconstruct

@docs remove, isEmpty, isBefore, isAfter, isFirst

# Util

@docs filterDuplicates

-}


{-| An arrangement of things in an order. An `UniqueList` is kind of like a `List`, except there cant be duplicate elements. And, its kind of like a `Set`, except an `UniqueList`s elements go from first to last.
-}
type UniqueList a
    = UniqueList (List a)

{-| Create a `UniqueList` from a `List`
-}
fromList : List a -> UniqueList a
fromList list =
    UniqueList (List.foldr consIfNotMember [] list)


{-| Turn a `UniqueList` into a `List`

    
    [1] == (List.Unique.toList << List.Unique.fromList) [1,1]

-}
toList : UniqueList a -> List a
toList (UniqueList list) =
    list


{-| Create an empty `UniqueList`

    List.Unique.empty == List.Unique.fromList []

-}
empty : UniqueList a
empty =
    UniqueList []


{-| Check if an Order is empty

    Order.isEmpty Order.empty == True

-}
isEmpty : UniqueList a -> Bool
isEmpty (UniqueList list) =
    List.isEmpty list


{-| Get the length of a `UniqueList`

    nobleGases = List.Unique.fromList
        [ helium
        , neon
        , argon
        , krypton
        , xenon
        , radon
        ]

    List.Unique.length nobleGases == 6

-}
length : UniqueList a -> Int
length (UniqueList list) =
    List.length list


{-| Reverse a `UniqueList`

    bestCoffeeDrinks ==
        List.Unique.fromList
            [ cortado
            , latte
            , coldbrew
            , coffee
            , mocha
            ]

    worstCoffeeDrinks : UniqueList Coffee
    worstCoffeeDrinks =
        List.Unique.reverse bestCoffeeDrinks

-}
reverse : UniqueList a -> UniqueList a
reverse (UniqueList list) =
    UniqueList (List.reverse list)


{-| Check if something is in a `UniqueList`

    List.Unique.member "Grover Cleveland" usPresidents == True

-}
member : a -> UniqueList a -> Bool
member el (UniqueList list) =
    List.member el list




{-| Remove an element from a `UniqueList`
-}
remove : a -> UniqueList a -> UniqueList a
remove element (UniqueList list) =
    UniqueList (filterFor element list)


{-| Add an element to the beginning of a `UniqueList`
-}
cons : a -> UniqueList a -> UniqueList a
cons element (UniqueList list) =
    UniqueList (element :: filterFor element list)


{-| Add an element to a `UniqueList` before another element

    addBefore `c` `b` (List.Unique.fromList [ `a`, `c`, `d` ])
        == List.Unique.fromList [ `a`, `b`, `c`, `d` ]

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


{-| Add an element to a `UniqueList` before another element

    addAfter `b` `c` (List.Unique.fromList [ `a`, `b`, `d` ])
        == List.Unique.fromList [ `a`, `b`, `c`, `d` ]

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


{-| Check if an element is before another, if it is in the `UniqueList` at all.

    germanStates = List.Unique.fromList [ "Bavaria", "Brandenberg" ]

    ("Bavaria" |> List.Unique.isBefore "Brandenberg") germanStates == Just True
    
    ("Brandenburg" |> List.Unique.isBefore "Bavaria") germanStates == Just False

    ("Bavaria" |> List.Unique.isBefore "New York City") germanStates == Nothing

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


{-| Check if an element is after another, if it is in the `UniqueList` at all.
-}
isAfter : a -> a -> UniqueList a -> Maybe Bool
isAfter first after order =
    isBefore after first order


{-| Check is an element is the first in a `UniqueList`
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


{-| If you just need to filter out duplicates, but dont want to use th `UniqueList` type, use this function. Its just like the `unique` function in `elm-community/list-extra`, except for the fact that `List.Extra.unique` only works on `List comparable`.

    filterDuplicates [ True, True ] == [ True ]

    filterDuplicates [ 1, 1, 2, 3, 5 ] == [ 1, 2, 3, 5 ]

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
