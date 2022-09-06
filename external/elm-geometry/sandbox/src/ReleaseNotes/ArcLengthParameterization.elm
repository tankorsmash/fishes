--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module ReleaseNotes.ArcLengthParameterization exposing (main)

import CubicSpline2d
import Drawing2d
import Geometry.Accuracy as Accuracy
import Html exposing (Html)
import ReleaseNotes.Common exposing (..)


main : Html Never
main =
    let
        parameterizedSpline =
            CubicSpline2d.arcLengthParameterized (Accuracy.maxError 0.5) spline

        overallArcLength =
            CubicSpline2d.arcLength parameterizedSpline

        arcLengths =
            List.range 0 numSegments
                |> List.map
                    (\n ->
                        let
                            fraction =
                                toFloat n
                                    / toFloat numSegments
                        in
                        fraction * overallArcLength
                    )

        points =
            arcLengths
                |> List.filterMap (CubicSpline2d.pointAlong parameterizedSpline)
    in
    Drawing2d.toHtml renderBounds
        []
        [ Drawing2d.cubicSpline spline
        , Drawing2d.dots points
        ]
