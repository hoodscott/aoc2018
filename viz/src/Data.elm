module Data exposing (Coords, palette, points1D, points2D, points3D)

import Color exposing (Color, rgb255)
import Length exposing (Meters)
import Point3d exposing (Point3d, meters)


type Coords
    = Coords


type alias Colours =
    { secondary : Color
    , tertiary : Color
    , tertiaryAlt : Color
    , red : Color
    , blue : Color
    , green : Color
    , purple : Color
    , orange : Color
    , pink : Color
    }


palette : Colours
palette =
    { secondary = rgb255 185 185 203
    , tertiary = rgb255 236 236 19
    , tertiaryAlt = rgb255 251 251 208
    , red = rgb255 236 19 19
    , blue = rgb255 19 19 236
    , green = rgb255 19 236 19
    , purple = rgb255 200 19 236
    , orange = rgb255 236 135 19
    , pink = rgb255 236 19 214
    }


points3D : List (Point3d Meters Coords)
points3D =
    List.map (\( x, y, z ) -> meters x y z) randomPoints


points2D : List (Point3d Meters Coords)
points2D =
    List.map (\( x, y, _ ) -> meters x y 0) randomPoints


points1D : List (Point3d Meters Coords)
points1D =
    List.map (\( x, _, _ ) -> meters x 0 0) randomPoints


randomPoints : List ( Float, Float, Float )
randomPoints =
    [ ( -14, -27, 25 )
    , ( 44, 38, 5 )
    , ( -50, -23, -13 )
    , ( 48, 4, -19 )
    , ( -23, 36, 44 )
    , ( -50, 10, -34 )
    , ( -18, 22, 0 )
    , ( -1, -2, 35 )
    , ( -43, -3, 0 )
    , ( -28, -11, 6 )
    , ( -2, -9, 4 )
    , ( 13, -27, 47 )
    , ( 26, -25, 11 )
    , ( -20, 22, -4 )
    , ( -11, 6, 31 )
    , ( -18, -34, 42 )
    , ( 1, 26, 12 )
    , ( 44, -37, 6 )
    , ( 42, -30, -11 )
    , ( 1, -50, -27 )
    , ( 6, -32, -11 )
    , ( 50, 9, 9 )
    , ( -2, 48, 7 )
    , ( -21, -28, -11 )
    , ( -7, 27, -23 )
    , ( -45, 19, 11 )
    , ( 30, 37, 26 )
    , ( 24, -35, -44 )
    , ( -11, -13, 40 )
    , ( 14, 27, 22 )
    , ( -41, -19, -8 )
    , ( -49, 36, -42 )
    , ( 48, 4, 41 )
    , ( 23, -12, -47 )
    , ( -30, 28, 34 )
    , ( -44, -38, -47 )
    , ( -47, -13, -49 )
    , ( 28, -5, 30 )
    , ( -20, 10, 30 )
    , ( 38, 41, -12 )
    , ( -4, -47, -3 )
    , ( -22, -28, -9 )
    , ( -8, 27, 31 )
    , ( 25, 17, -24 )
    , ( -7, -43, 3 )
    , ( 48, -6, 15 )
    , ( 3, 42, 36 )
    , ( 42, -36, 49 )
    , ( 16, 20, 30 )
    , ( -11, 0, -28 )
    , ( 25, 34, 10 )
    , ( 45, -49, 28 )
    , ( -30, -12, -30 )
    , ( -50, -41, 10 )
    , ( -20, 15, -50 )
    , ( -35, 6, -1 )
    , ( 34, 39, -44 )
    , ( 25, 13, 10 )
    , ( 39, 43, 14 )
    , ( -18, -16, 39 )
    , ( 39, -36, 11 )
    , ( 16, 3, -33 )
    , ( -32, -21, -40 )
    , ( -10, -32, 48 )
    , ( -35, 47, 32 )
    , ( 12, -18, 20 )
    , ( 49, -26, 32 )
    , ( 22, -46, 40 )
    , ( 16, -46, -42 )
    , ( -13, -48, -26 )
    , ( -34, -18, 20 )
    , ( -8, 33, 4 )
    , ( -3, -33, -47 )
    , ( 35, -7, -27 )
    , ( -19, 19, 42 )
    , ( 45, 30, -20 )
    , ( -20, -40, -13 )
    , ( -16, 17, 0 )
    , ( 22, -9, 47 )
    , ( 0, 41, 24 )
    , ( -36, 41, 2 )
    , ( -14, 23, 5 )
    , ( -35, 32, 20 )
    , ( -6, 3, 13 )
    , ( -43, 23, 49 )
    , ( 0, 45, 25 )
    , ( 36, 15, -29 )
    , ( -28, 7, -44 )
    , ( -3, 43, -43 )
    , ( 31, 50, 22 )
    , ( -42, 46, 12 )
    , ( -2, -6, -16 )
    , ( -41, 13, -26 )
    , ( -33, 21, 36 )
    , ( -33, 6, 1 )
    , ( -32, -37, -38 )
    , ( -15, 24, 1 )
    , ( 1, -48, 22 )
    , ( 33, 7, 46 )
    , ( -20, -18, -27 )
    , ( -43, 11, -50 )
    ]
