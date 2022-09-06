module Tests exposing
    ( abs
    , aggregate3
    , aggregateN
    , cosWorksProperly
    , cubed
    , difference
    , divideBy
    , hull3
    , hullN
    , interpolationParameter
    , intersection
    , intersectsAndIntersectionAreConsistent
    , minus
    , minusInterval
    , multiplyBy
    , plus
    , plusInterval
    , product
    , sinWorksProperly
    , squared
    , times
    , timesInterval
    , timesUnitless
    , timesUnitlessInterval
    , union
    )

import Angle exposing (Radians)
import Angle.Interval
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Quantity exposing (Quantity, Unitless)
import Quantity.Interval as Interval exposing (Interval)
import Test exposing (Test)


quantityFuzzer : Fuzzer (Quantity Float Unitless)
quantityFuzzer =
    Fuzz.map Quantity.float (Fuzz.floatRange -10 10)


intervalFuzzer : Fuzzer (Interval Float Unitless)
intervalFuzzer =
    Fuzz.map2 Interval.from quantityFuzzer quantityFuzzer


angleIntervalFuzzer : Fuzzer (Interval Float Radians)
angleIntervalFuzzer =
    let
        endpoint =
            Fuzz.map Angle.radians (Fuzz.floatRange (-2 * pi) (2 * pi))
    in
    Fuzz.map2 Interval.from endpoint endpoint


endpointsString : Interval Float units -> String
endpointsString interval =
    let
        ( minValue, maxValue ) =
            Interval.endpoints interval
    in
    String.concat
        [ "["
        , String.fromFloat (Quantity.unwrap minValue)
        , ","
        , String.fromFloat (Quantity.unwrap maxValue)
        , "]"
        ]


expectValueIn : Interval Float units -> Quantity Float units -> Expectation
expectValueIn interval value =
    let
        ( low, high ) =
            Interval.endpoints interval

        tolerance =
            Quantity.unsafe 1.0e-12
    in
    if
        (value |> Quantity.greaterThanOrEqualTo (low |> Quantity.minus tolerance))
            && (value |> Quantity.lessThanOrEqualTo (high |> Quantity.plus tolerance))
    then
        Expect.pass

    else
        Expect.fail
            (String.fromFloat (Quantity.unwrap value)
                ++ " is not contained in the interval "
                ++ endpointsString interval
            )


sinWorksProperly : Test
sinWorksProperly =
    Test.fuzz2
        angleIntervalFuzzer
        (Fuzz.floatRange 0 1)
        "sin works as expected"
        (\interval t ->
            let
                valueInInterval =
                    Interval.interpolate interval t
            in
            Quantity.float (Angle.sin valueInInterval)
                |> expectValueIn
                    (Angle.Interval.sin interval)
        )


cosWorksProperly : Test
cosWorksProperly =
    Test.fuzz2
        angleIntervalFuzzer
        (Fuzz.floatRange 0 1)
        "cos works as expected"
        (\interval t ->
            let
                valueInInterval =
                    Interval.interpolate interval t
            in
            Quantity.float (Angle.cos valueInInterval)
                |> expectValueIn
                    (Angle.Interval.cos interval)
        )


hullN : Test
hullN =
    Test.fuzz (Fuzz.list (Fuzz.map Quantity.float Fuzz.float))
        "hullN"
        (\values ->
            case Interval.hullN values of
                Just interval ->
                    interval
                        |> Expect.all
                            (List.map
                                (\value ->
                                    \theInterval ->
                                        value |> expectValueIn theInterval
                                )
                                values
                            )

                Nothing ->
                    values |> Expect.equal []
        )


union : Test
union =
    Test.describe "union"
        [ Test.fuzz3
            intervalFuzzer
            intervalFuzzer
            (Fuzz.floatRange 0 1)
            "Value in first interval is in union"
            (\firstInterval secondInterval t ->
                Interval.interpolate firstInterval t
                    |> expectValueIn
                        (Interval.union firstInterval secondInterval)
            )
        , Test.fuzz3
            intervalFuzzer
            intervalFuzzer
            (Fuzz.floatRange 0 1)
            "Value in second interval is in union"
            (\firstInterval secondInterval t ->
                Interval.interpolate secondInterval t
                    |> expectValueIn
                        (Interval.union firstInterval secondInterval)
            )
        ]


expectDistinct : Interval Float Unitless -> Interval Float Unitless -> Expectation
expectDistinct firstInterval secondInterval =
    if firstInterval |> Interval.intersects secondInterval then
        Expect.fail <|
            "Intervals "
                ++ endpointsString firstInterval
                ++ " and "
                ++ endpointsString secondInterval
                ++ " are not distinct"

    else
        Expect.pass


intersection : Test
intersection =
    Test.fuzz3
        intervalFuzzer
        intervalFuzzer
        (Fuzz.floatRange 0 1)
        "Value in intersection is in both intervals"
        (\firstInterval secondInterval t ->
            case Interval.intersection firstInterval secondInterval of
                Just intersectionInterval ->
                    Interval.interpolate intersectionInterval t
                        |> Expect.all
                            [ expectValueIn firstInterval
                            , expectValueIn secondInterval
                            ]

                Nothing ->
                    expectDistinct firstInterval secondInterval
        )


expectContainedIn : Interval Float Unitless -> Interval Float Unitless -> Expectation
expectContainedIn firstInterval secondInterval =
    if Interval.isContainedIn firstInterval secondInterval then
        Expect.pass

    else
        Expect.fail <|
            "Interval "
                ++ endpointsString secondInterval
                ++ " is not contained in "
                ++ endpointsString firstInterval


aggregateN : Test
aggregateN =
    Test.fuzz (Fuzz.list intervalFuzzer)
        "aggregateN"
        (\intervals ->
            case Interval.aggregateN intervals of
                Just aggregateInterval ->
                    aggregateInterval
                        |> Expect.all
                            (List.map
                                (\interval ->
                                    \theAggregateInterval ->
                                        interval
                                            |> expectContainedIn
                                                theAggregateInterval
                                )
                                intervals
                            )

                Nothing ->
                    intervals |> Expect.equal []
        )


intersectsAndIntersectionAreConsistent : Test
intersectsAndIntersectionAreConsistent =
    Test.fuzz2
        intervalFuzzer
        intervalFuzzer
        "intersects and intersection are consistent"
        (\firstInterval secondInterval ->
            let
                intersects =
                    Interval.intersects firstInterval secondInterval

                maybeIntersection =
                    Interval.intersection firstInterval secondInterval
            in
            intersects |> Expect.equal (maybeIntersection /= Nothing)
        )


testScalarOperation :
    String
    -> Fuzzer Float
    -> (Float -> Quantity Float Unitless -> Quantity Float Unitless)
    -> (Float -> Interval Float Unitless -> Interval Float Unitless)
    -> Test
testScalarOperation description scalarFuzzer scalarFunction intervalFunction =
    Test.fuzz3
        scalarFuzzer
        intervalFuzzer
        (Fuzz.floatRange 0 1)
        description
        (\scalar interval t ->
            let
                valueInInterval =
                    Interval.interpolate interval t
            in
            scalarFunction scalar valueInInterval
                |> expectValueIn
                    (intervalFunction scalar interval)
        )


testQuantityOperation :
    String
    -> (Quantity Float Unitless -> Quantity Float Unitless -> Quantity Float resultUnits)
    -> (Quantity Float Unitless -> Interval Float Unitless -> Interval Float resultUnits)
    -> Test
testQuantityOperation description quantityFunction intervalFunction =
    Test.fuzz3
        quantityFuzzer
        intervalFuzzer
        (Fuzz.floatRange 0 1)
        description
        (\quantity interval t ->
            let
                valueInInterval =
                    Interval.interpolate interval t
            in
            quantityFunction quantity valueInInterval
                |> expectValueIn
                    (intervalFunction quantity interval)
        )


testUnaryOperation :
    String
    -> (Quantity Float Unitless -> Quantity Float resultUnits)
    -> (Interval Float Unitless -> Interval Float resultUnits)
    -> Test
testUnaryOperation description quantityFunction intervalFunction =
    Test.fuzz2
        intervalFuzzer
        (Fuzz.floatRange 0 1)
        description
        (\interval u ->
            let
                value =
                    Interval.interpolate interval u
            in
            quantityFunction value |> expectValueIn (intervalFunction interval)
        )


testBinaryOperation :
    String
    -> (Quantity Float Unitless -> Quantity Float Unitless -> Quantity Float resultUnits)
    -> (Interval Float Unitless -> Interval Float Unitless -> Interval Float resultUnits)
    -> Test
testBinaryOperation description quantityFunction intervalFunction =
    Test.fuzz2
        (Fuzz.tuple ( intervalFuzzer, Fuzz.floatRange 0 1 ))
        (Fuzz.tuple ( intervalFuzzer, Fuzz.floatRange 0 1 ))
        description
        (\( firstInterval, u ) ( secondInterval, v ) ->
            let
                valueInFirstInterval =
                    Interval.interpolate firstInterval u

                valueInSecondInterval =
                    Interval.interpolate secondInterval v
            in
            quantityFunction valueInFirstInterval valueInSecondInterval
                |> expectValueIn
                    (intervalFunction firstInterval secondInterval)
        )


plus : Test
plus =
    testQuantityOperation "plus" Quantity.plus Interval.plus


minus : Test
minus =
    testQuantityOperation "minus" Quantity.minus Interval.minus


difference : Test
difference =
    testQuantityOperation "difference" (\a b -> a |> Quantity.minus b) Interval.difference


multiplyBy : Test
multiplyBy =
    testScalarOperation "multiplyBy"
        (Fuzz.floatRange -10 10)
        Quantity.multiplyBy
        Interval.multiplyBy


divideBy : Test
divideBy =
    testScalarOperation "divideBy"
        (Fuzz.oneOf [ Fuzz.floatRange -10 -0.1, Fuzz.floatRange 0.1 10 ])
        Quantity.divideBy
        Interval.divideBy


plusInterval : Test
plusInterval =
    testBinaryOperation "plusInterval" Quantity.plus Interval.plusInterval


minusInterval : Test
minusInterval =
    testBinaryOperation "minusInterval" Quantity.minus Interval.minusInterval


times : Test
times =
    testQuantityOperation "times" Quantity.times Interval.times


timesUnitless : Test
timesUnitless =
    testQuantityOperation "timesUnitless" Quantity.timesUnitless Interval.timesUnitless


product : Test
product =
    testQuantityOperation "product" Quantity.times Interval.product


timesInterval : Test
timesInterval =
    testBinaryOperation "timesInterval" Quantity.times Interval.timesInterval


timesUnitlessInterval : Test
timesUnitlessInterval =
    testBinaryOperation "timesUnitlessInterval" Quantity.timesUnitless Interval.timesUnitlessInterval


abs : Test
abs =
    testUnaryOperation "abs" Quantity.abs Interval.abs


squared : Test
squared =
    testUnaryOperation "squared" Quantity.squared Interval.squared


cubed : Test
cubed =
    testUnaryOperation "cubed" Quantity.cubed Interval.cubed


interpolationParameter : Test
interpolationParameter =
    Test.fuzz2
        intervalFuzzer
        quantityFuzzer
        "interpolationParameter"
        (\interval quantity ->
            if Interval.isSingleton interval then
                Expect.pass

            else
                Interval.interpolate interval
                    (Interval.interpolationParameter interval quantity)
                    |> Quantity.equalWithin (Quantity.float 1.0e-12) quantity
                    |> Expect.true "Expected interpolate and interpolationParameter to be inverses"
        )


hull3 : Test
hull3 =
    Test.fuzz3
        quantityFuzzer
        quantityFuzzer
        quantityFuzzer
        "hull3"
        (\a b c ->
            Interval.hull3 a b c
                |> Expect.equal (Interval.hull a [ b, c ])
        )


aggregate3 : Test
aggregate3 =
    Test.fuzz3
        intervalFuzzer
        intervalFuzzer
        intervalFuzzer
        "aggregate3"
        (\a b c ->
            Interval.aggregate3 a b c
                |> Expect.equal (Interval.aggregate a [ b, c ])
        )
