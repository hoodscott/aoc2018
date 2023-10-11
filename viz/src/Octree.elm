module Octree exposing (main)

import Angle exposing (Angle)
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Duration exposing (Duration)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Length
import LineSegment3d
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material
import Speed
import Vector3d
import Viewpoint3d


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


type Coords
    = Coords


type Dimension
    = Dim1D
    | Dim2D
    | Dim3D


type alias AnimatedEntity =
    { entity : Scene3d.Entity Coords
    , from : Point3d.Point3d Length.Meters Coords
    , to : Point3d.Point3d Length.Meters Coords
    , isMoving : Bool
    , colour : Color.Color
    }


type alias Model =
    { -- camera
      azimuth : Angle -- orbit angle of the camera around the focal point
    , elevation : Angle -- angle of the camera up from the XY plane
    , orbiting : Bool -- are we moving the camera

    -- problem simulation
    , dimension : Dimension -- how many dimensions are we showing
    , range : Int --

    -- scene3d entities
    , entities : List AnimatedEntity --  list of points as entities
    , axes : { x : Scene3d.Entity Coords, y : Scene3d.Entity Coords, z : Scene3d.Entity Coords }
    }


initialModel : Model
initialModel =
    let
        axisSize =
            65
    in
    { azimuth = Angle.degrees 225
    , elevation = Angle.degrees 30
    , orbiting = False
    , dimension = Dim3D
    , range = 0
    , entities = List.map createAnimatedEntity points3D
    , axes =
        { x =
            Scene3d.lineSegment (Scene3d.Material.color Color.red)
                (LineSegment3d.from (Point3d.meters -axisSize 0 0) (Point3d.meters axisSize 0 0))
        , y =
            Scene3d.lineSegment (Scene3d.Material.color Color.green)
                (LineSegment3d.from (Point3d.meters 0 -axisSize 0) (Point3d.meters 0 axisSize 0))
        , z =
            Scene3d.lineSegment (Scene3d.Material.color Color.blue)
                (LineSegment3d.from (Point3d.meters 0 0 -axisSize) (Point3d.meters 0 0 axisSize))
        }
    }


createAnimatedEntity : Point3d.Point3d Length.Meters Coords -> AnimatedEntity
createAnimatedEntity point =
    { entity = createPoint Color.white point, from = point, to = point, isMoving = False, colour = Color.white }


ifPositive : AnimatedEntity -> Bool
ifPositive entity =
    Length.inMeters (Point3d.xCoordinate entity.to) >= 0


ifNegative : AnimatedEntity -> Bool
ifNegative entity =
    Length.inMeters (Point3d.xCoordinate entity.to) < 0


highlight : (AnimatedEntity -> Bool) -> AnimatedEntity -> AnimatedEntity
highlight condition entity =
    let
        newColour =
            if condition entity then
                Color.yellow

            else
                Color.white
    in
    if newColour == entity.colour then
        entity

    else
        { entity | entity = createPoint newColour entity.from, colour = newColour }


createPoint : Color.Color -> Point3d.Point3d Length.Meters c -> Scene3d.Entity c
createPoint colour point =
    Scene3d.point { radius = Pixels.float 2.5 }
        (Scene3d.Material.color colour)
        point


init : () -> ( Model, Cmd msg )
init _ =
    ( initialModel, Cmd.none )


type Msg
    = --mouse stuff
      MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
      -- camera control
    | ToTopDownCamera
    | ToInitialCamera
      -- simulation stuff
    | NextD
    | HighlightPointsPos
    | HighlightPointsNeg
      -- animation stuff
    | Tick Duration


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    -- orbit the camera by 1 degree per pixel of drag
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- adjust azimuth according to horizontal mouse motion
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- adjust elevation according to vertical mouse motion
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)

                    -- clamp to make sure camera cannot go past vertical
                    -- in either direction
                    -- |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }, Cmd.none )

            else
                ( model, Cmd.none )

        NextD ->
            case model.dimension of
                Dim1D ->
                    ( { model | dimension = Dim2D, entities = newCoords model.entities points2D }, Cmd.none )

                Dim2D ->
                    ( { model | dimension = Dim3D, entities = newCoords model.entities points3D }, Cmd.none )

                Dim3D ->
                    ( { model | dimension = Dim1D, entities = newCoords model.entities points1D }, Cmd.none )

        Tick duration ->
            ( { model | entities = List.map (animateEntity duration) model.entities }, Cmd.none )

        ToTopDownCamera ->
            ( { model | azimuth = Angle.degrees 270, elevation = Angle.degrees 90 }, Cmd.none )

        ToInitialCamera ->
            ( { model | azimuth = Angle.degrees 225, elevation = Angle.degrees 30 }, Cmd.none )

        HighlightPointsPos ->
            ( { model | entities = List.map (\e -> highlight ifPositive e) model.entities }, Cmd.none )

        HighlightPointsNeg ->
            ( { model | entities = List.map (\e -> highlight ifNegative e) model.entities }, Cmd.none )


animateEntity : Duration -> AnimatedEntity -> AnimatedEntity
animateEntity duration entity =
    let
        difference =
            LineSegment3d.from entity.from entity.to

        direction : Maybe (Direction3d.Direction3d Coords)
        direction =
            LineSegment3d.direction difference
    in
    if not entity.isMoving then
        entity

    else if Length.inMeters (LineSegment3d.length difference) < 0.25 then
        { entity | entity = createPoint entity.colour entity.to, from = entity.to, isMoving = False }

    else
        case direction of
            Just dir ->
                let
                    length : Length.Length
                    length =
                        Quantity.at
                            (Speed.metersPerSecond
                                (if Length.inMeters (LineSegment3d.length difference) > 15 then
                                    100

                                 else if Length.inMeters (LineSegment3d.length difference) > 5 then
                                    50

                                 else
                                    25
                                )
                            )
                            duration

                    newFrom =
                        Point3d.translateBy (Vector3d.withLength length dir) entity.from

                    newEntity =
                        Scene3d.translateBy (Vector3d.withLength length dir) entity.entity
                in
                { entity | entity = newEntity, from = newFrom }

            Nothing ->
                entity


newCoords : List AnimatedEntity -> List (Point3d.Point3d Length.Meters Coords) -> List AnimatedEntity
newCoords oldCoords newPoints =
    let
        something entity newPoint =
            { entity | to = newPoint, isMoving = True }
    in
    List.map2 something oldCoords newPoints


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        (if model.orbiting then
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

         else
            []
        )
            ++ (if List.any needsAnimated model.entities then
                    [ Browser.Events.onAnimationFrameDelta
                        (Duration.milliseconds >> Tick)
                    ]

                else
                    []
               )


needsAnimated : AnimatedEntity -> Bool
needsAnimated entity =
    entity.isMoving


view : Model -> Html Msg
view model =
    let
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0.5 0.5 0
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 200
                }

        axes =
            case model.dimension of
                Dim1D ->
                    [ model.axes.x ]

                Dim2D ->
                    [ model.axes.x, model.axes.y ]

                Dim3D ->
                    [ model.axes.x, model.axes.y, model.axes.z ]
    in
    main_
        [ style "box-sizing" "border-box"
        , style "width" "100%"
        , style "height" "100vh"
        , style "padding" "1rem"
        , style "display" "flex"
        , style "gap" "1rem"
        , style "flex-wrap" "wrap"
        , style "align-items" "start"
        , style "background" "dimgrey"
        ]
        [ section
            [ Html.Events.onMouseDown MouseDown
            , style "border" "1px solid black"
            , style "background" "slategrey"
            , style "cursor" "move"
            ]
            [ Scene3d.unlit
                { camera =
                    Camera3d.perspective
                        { viewpoint = viewpoint
                        , verticalFieldOfView = Angle.degrees 30
                        }
                , clipDepth = Length.meters 0.1
                , dimensions = ( Pixels.int 800, Pixels.int 600 )
                , background = Scene3d.transparentBackground
                , entities = axes ++ List.map .entity model.entities
                }
            ]
        , section
            [ style "flex" "1"
            , style "display" "flex"
            , style "flex-wrap" "wrap"
            ]
            [ button [ onClick NextD ] [ text "next dim" ]
            , button [ onClick ToTopDownCamera ] [ text "top down" ]
            , button [ onClick ToInitialCamera ] [ text "fourtyfive" ]
            , button [ onClick HighlightPointsPos ] [ text "make positive x yellow" ]
            , button [ onClick HighlightPointsNeg ] [ text "make negative x yellow" ]
            ]
        ]


randomPoints : List ( Float, Float, Float )
randomPoints =
    [ ( -14, -27, 25 ), ( 44, 38, 5 ), ( -50, -23, -13 ), ( 48, 4, -19 ), ( -23, 36, 44 ), ( -50, 10, -34 ), ( -18, 22, 0 ), ( -1, -2, 35 ), ( -43, -3, 0 ), ( -28, -11, 6 ), ( -2, -9, 4 ), ( 13, -27, 47 ), ( 26, -25, 11 ), ( -20, 22, -4 ), ( -11, 6, 31 ), ( -18, -34, 42 ), ( 1, 26, 12 ), ( 44, -37, 6 ), ( 42, -30, -11 ), ( 1, -50, -27 ), ( 6, -32, -11 ), ( 50, 9, 9 ), ( -2, 48, 7 ), ( -21, -28, -11 ), ( -7, 27, -23 ), ( -45, 19, 11 ), ( 30, 37, 26 ), ( 24, -35, -44 ), ( -11, -13, 40 ), ( 14, 27, 22 ), ( -41, -19, -8 ), ( -49, 36, -42 ), ( 48, 4, 41 ), ( 23, -12, -47 ), ( -30, 28, 34 ), ( -44, -38, -47 ), ( -47, -13, -49 ), ( 28, -5, 30 ), ( -20, 10, 30 ), ( 38, 41, -12 ), ( -4, -47, -3 ), ( -22, -28, -9 ), ( -8, 27, 31 ), ( 25, 17, -24 ), ( -7, -43, 3 ), ( 48, -6, 15 ), ( 3, 42, 36 ), ( 42, -36, 49 ), ( 16, 20, 30 ), ( -11, 0, -28 ), ( 25, 34, 10 ), ( 45, -49, 28 ), ( -30, -12, -30 ), ( -50, -41, 10 ), ( -20, 15, -50 ), ( -35, 6, -1 ), ( 34, 39, -44 ), ( 25, 13, 10 ), ( 39, 43, 14 ), ( -18, -16, 39 ), ( 39, -36, 11 ), ( 16, 3, -33 ), ( -32, -21, -40 ), ( -10, -32, 48 ), ( -35, 47, 32 ), ( 12, -18, 20 ), ( 49, -26, 32 ), ( 22, -46, 40 ), ( 16, -46, -42 ), ( -13, -48, -26 ), ( -34, -18, 20 ), ( -8, 33, 4 ), ( -3, -33, -47 ), ( 35, -7, -27 ), ( -19, 19, 42 ), ( 45, 30, -20 ), ( -20, -40, -13 ), ( -16, 17, 0 ), ( 22, -9, 47 ), ( 0, 41, 24 ), ( -36, 41, 2 ), ( -14, 23, 5 ), ( -35, 32, 20 ), ( -6, 3, 13 ), ( -43, 23, 49 ), ( 0, 45, 25 ), ( 36, 15, -29 ), ( -28, 7, -44 ), ( -3, 43, -43 ), ( 31, 50, 22 ), ( -42, 46, 12 ), ( -2, -6, -16 ), ( -41, 13, -26 ), ( -33, 21, 36 ), ( -33, 6, 1 ), ( -32, -37, -38 ), ( -15, 24, 1 ), ( 1, -48, 22 ), ( 33, 7, 46 ), ( -20, -18, -27 ), ( -43, 11, -50 ) ]


points3D : List (Point3d.Point3d Length.Meters Coords)
points3D =
    List.map (\( x, y, z ) -> Point3d.meters x y z) randomPoints


points2D : List (Point3d.Point3d Length.Meters Coords)
points2D =
    List.map (\( x, y, _ ) -> Point3d.meters x y 0) randomPoints


points1D : List (Point3d.Point3d Length.Meters Coords)
points1D =
    List.map (\( x, _, _ ) -> Point3d.meters x 0 0) randomPoints
