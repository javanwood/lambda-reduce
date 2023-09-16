module LambdaTests exposing (tests)

import Dict
import Expect
import Lambda
import Lambda.StdEnv as StdEnv
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Lambda Tests"
        [ test "Id function applied to arg" <|
            \_ ->
                let
                    result =
                        Lambda.apply
                            (Lambda.init "x" [] (Lambda.Free "x"))
                            [ Lambda.Free "a" ]
                            |> Lambda.headNF
                in
                Expect.equal (Lambda.Free "a") result
        , test "If conditionals" <|
            \_ ->
                let
                    eval lambda val =
                        Lambda.instantiate (Dict.singleton "p" val) lambda |> Lambda.headNF
                in
                Expect.all
                    [ \l -> Expect.equal (Lambda.Free "a") (eval l StdEnv.true)
                    , \l -> Expect.equal (Lambda.Free "b") (eval l StdEnv.false)
                    ]
                    (Lambda.apply StdEnv.if_ [ Lambda.Free "p", Lambda.Free "a", Lambda.Free "b" ])
        , test "Lambda.Abst is variable binding" <|
            \_ ->
                Expect.equal
                    (Lambda.Abst "a" (Lambda.Bound 0))
                    (Lambda.init "a" [] (Lambda.Free "a"))
        , test "Lambda.Abst is counts number of bindings above." <|
            \_ ->
                Expect.equal
                    (Lambda.Abst "a" (Lambda.Abst "b" (Lambda.Bound 1)))
                    (Lambda.init "a" [ "b" ] (Lambda.Free "a"))
        , test "Lambda.Free is for free variables" <|
            \_ ->
                Expect.equal
                    (Lambda.Abst "a" (Lambda.Abst "b" (Lambda.Apply (Lambda.Free "f") (Lambda.Bound 1))))
                    (Lambda.init "a" [ "b" ] (Lambda.apply (Lambda.Free "f") [ Lambda.Free "a" ]))
        , test "Lambda-based record type" <|
            \_ ->
                let
                    eval lambda val =
                        Lambda.apply lambda [ val ] |> Lambda.headNF

                    record4 =
                        Lambda.init "p1"
                            [ "p2", "p3", "p4", "f" ]
                            (Lambda.apply (Lambda.Free "f")
                                [ Lambda.Free "p1"
                                , Lambda.Free "p2"
                                , Lambda.Free "p3"
                                , Lambda.Free "p4"
                                ]
                            )

                    get1 =
                        Lambda.init "p1" [ "p2", "p3", "p4" ] (Lambda.Free "p1")

                    get2 =
                        Lambda.init "p1" [ "p2", "p3", "p4" ] (Lambda.Free "p2")

                    get3 =
                        Lambda.init "p1" [ "p2", "p3", "p4" ] (Lambda.Free "p3")

                    get4 =
                        Lambda.init "p1" [ "p2", "p3", "p4" ] (Lambda.Free "p4")
                in
                Expect.all
                    [ \record -> Expect.equal (Lambda.Free "a") (eval record get1)
                    , \record -> Expect.equal (Lambda.Free "b") (eval record get2)
                    , \record -> Expect.equal (Lambda.Free "c") (eval record get3)
                    , \record -> Expect.equal (Lambda.Free "d") (eval record get4)
                    ]
                    (Lambda.apply record4
                        [ Lambda.Free "a"
                        , Lambda.Free "b"
                        , Lambda.Free "c"
                        , Lambda.Free "d"
                        ]
                    )
        , test "Double substitution" <|
            \_ ->
                let
                    expr =
                        Lambda.apply (Lambda.Free "paint") [ Lambda.Free "x" ]

                    env =
                        Dict.fromList
                            [ ( "paint", Lambda.Free "interpolate" )
                            , ( "interpolate", Lambda.Free "user-defined" )
                            ]
                in
                Expect.equal
                    (Lambda.apply (Lambda.Free "user-defined") [ Lambda.Free "x" ])
                    (Lambda.instantiate env expr |> Lambda.headNF)
        ]
