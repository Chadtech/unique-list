module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple3)
import Order
import Debug exposing (log)


all : Test
all =
    describe "Bool.Extra"
        [ describe "toList, fromList"
            [ test "list is the same after toList and fromList" <|
                \() ->
                    Expect.true 
                        "(Order.toList <| Order.fromList [ 1, 2, 3 ]) == [ 1, 2, 3 ]"
                        ((Order.toList <| Order.fromList [ 1, 2, 3 ]) == [ 1, 2, 3 ])
            , test "toList removed duplicates" <|
                \() ->
                    Expect.true
                        "(Order.toList <| Order.fromList [ 1, 1, 2, 3 ]) == [ 1, 2, 3 ]"
                        ((Order.toList <| Order.fromList [ 1, 1, 2, 3 ]) == [ 1, 2, 3 ])
            , test "empty toList is an empty list" <|
                \() ->
                    Expect.equal (Order.toList Order.empty) []
            , test "remove 'a' from alphabet" <|
                \() ->
                    Expect.equal
                        ((Order.toList << Order.remove 'a') (Order.fromList ['a', 'b', 'c']))
                         ['b', 'c']
            , test "remove 'b' from alphabet" <|
                \() ->
                    Expect.equal
                        ((Order.toList << Order.remove 'b') (Order.fromList ['a', 'b', 'c']))
                         ['a', 'c']
            , test "getOrderOf 'c'" <|
                \() ->
                    Expect.equal
                        (Order.getOrderOf 'b' (Order.fromList [ 'a', 'b', 'c' ]))
                        (Just 1)
            , test "getOrderOf element not in order" <|
                \() ->
                    Expect.equal
                        (Order.getOrderOf 'f' (Order.fromList [ 'a', 'b', 'c' ]))
                        Nothing
            , test "'a' is added to beginning" <|
                \() ->
                    Expect.equal
                        ((Order.toList << (Order.addFirst 'a')) (Order.fromList [ 'b', 'c']))
                        [ 'a', 'b', 'c' ]
            , test "Order.addBefore, 'a' is added to before 'b'" <|
                \() ->
                    Expect.equal
                        ((Order.toList << Order.addBefore 'b' 'a') (Order.fromList [ 'b', 'c']))
                        [ 'a', 'b', 'c' ]
            , test "'b' is added after 'a'" <|
                \() ->
                    Expect.equal
                        ((Order.toList << ('b' |> Order.addAfter 'a')) (Order.fromList [ 'a', 'c' ]))
                        [ 'a', 'b', 'c' ]
            , test "'a' is before 'b'" <|
                \() ->
                    Expect.equal (Order.isBefore 'b' 'a' (Order.fromList [ 'a', 'b', 'c' ]))
                    (Just True)
            , test "'z' is before 'b' in abc (should be Nothing)" <|
                \() ->
                    Expect.equal (Order.isBefore 'b' 'z' (Order.fromList ['a', 'b', 'c'])) 
                    Nothing
            , test "order of 'a' is Just 0" <|
                \() ->
                    Expect.equal (Order.getOrderOf 'a' (Order.fromList ['a', 'b', 'c']))
                    (Just 0)
            , test "order of 'b' is Just 1" <|
                \() ->
                    Expect.equal (Order.getOrderOf 'b' (Order.fromList ['a', 'b', 'c']))
                    (Just 1)
            , test "order of 'z' is Nothing" <|
                \() ->
                    Expect.equal (Order.getOrderOf 'z' (Order.fromList ['a', 'b', 'c']))
                    Nothing
            ]
        ]

