module Octree exposing (main)

import Angle
import Axis3d
import Block3d
import BoundingBox3d
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Duration exposing (Duration)
import Html exposing (..)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onClick)
import Html.Events.Extra.Wheel
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


type alias Target =
    { position : Point3d.Point3d Length.Meters Coords
    , speedMult : Float
    }


type alias AnimatedPoint =
    { position : Point3d.Point3d Length.Meters Coords
    , futurePositions : List Target
    , colour : Color.Color
    }


type alias Model =
    { -- camera
      azimuth : Angle.Angle -- orbit angle of the camera around the focal point
    , elevation : Angle.Angle -- angle of the camera up from the XY plane
    , distance : Length.Length
    , orbiting : Bool -- are we moving the camera
    , orthographic : Bool

    -- problem simulation
    , dimension : Dimension -- how many dimensions are we showing
    , range : Float
    , showSubBox : Bool
    , subBoxes : List (Block3d.Block3d Length.Meters Coords)

    -- scene3d entities
    , points : List AnimatedPoint -- list of points
    , box : Maybe (Block3d.Block3d Length.Meters Coords)
    , colourEdges : Bool
    , showRhombiDodec : Bool
    , showTetrakis : Bool
    , showRhombiCube : Bool
    , axes :
        { x : Scene3d.Entity Coords
        , y : Scene3d.Entity Coords
        , z : Scene3d.Entity Coords
        }
    }


initialModel : Model
initialModel =
    let
        axisSize =
            65
    in
    { azimuth = Angle.degrees 225
    , elevation = Angle.degrees 30
    , distance = Length.meters 200
    , orbiting = False
    , orthographic = True
    , dimension = Dim3D
    , range = 4
    , showSubBox = False
    , subBoxes = []
    , points = List.map createAnimatedEntity points3D
    , box = Nothing
    , colourEdges = False
    , showRhombiDodec = False
    , showTetrakis = False
    , showRhombiCube = True
    , axes =
        { x =
            Scene3d.lineSegment (Scene3d.Material.color Color.red) <|
                LineSegment3d.from
                    (Point3d.meters -axisSize 0 0)
                    (Point3d.meters axisSize 0 0)
        , y =
            Scene3d.lineSegment (Scene3d.Material.color Color.green) <|
                LineSegment3d.from
                    (Point3d.meters 0 -axisSize 0)
                    (Point3d.meters 0 axisSize 0)
        , z =
            Scene3d.lineSegment (Scene3d.Material.color Color.blue) <|
                LineSegment3d.from
                    (Point3d.meters 0 0 -axisSize)
                    (Point3d.meters 0 0 axisSize)
        }
    }


createAnimatedEntity : Point3d.Point3d Length.Meters Coords -> AnimatedPoint
createAnimatedEntity point =
    { position = point, futurePositions = [], colour = Color.white }


ifPositive : AnimatedPoint -> Bool
ifPositive entity =
    Length.inMeters (Point3d.xCoordinate entity.position) >= 0


ifNegative : AnimatedPoint -> Bool
ifNegative entity =
    Length.inMeters (Point3d.xCoordinate entity.position) < 0


ifInRangeofBox :
    Float
    -> Maybe (Block3d.Block3d Length.Meters Coords)
    -> AnimatedPoint
    -> Bool
ifInRangeofBox range maybeBox entity =
    case maybeBox of
        Just box ->
            if Block3d.contains entity.position box then
                True

            else
                let
                    boundingBox =
                        Block3d.boundingBox box
                            |> BoundingBox3d.extrema

                    diff min max test =
                        if Quantity.lessThan min test then
                            Quantity.difference min test

                        else if Quantity.greaterThan max test then
                            Quantity.difference test max

                        else
                            Length.meters 0

                    xOff =
                        Point3d.xCoordinate entity.position
                            |> diff boundingBox.minX boundingBox.maxX

                    yOff =
                        Point3d.yCoordinate entity.position
                            |> diff boundingBox.minY boundingBox.maxY

                    zOff =
                        Point3d.zCoordinate entity.position
                            |> diff boundingBox.minZ boundingBox.maxZ
                in
                Quantity.sum [ xOff, yOff, zOff ]
                    |> Quantity.lessThanOrEqualTo (Length.meters range)

        Nothing ->
            False


highlight : (AnimatedPoint -> Bool) -> AnimatedPoint -> AnimatedPoint
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
        { entity | colour = newColour }


createSceneEntity : AnimatedPoint -> Scene3d.Entity Coords
createSceneEntity entity =
    Scene3d.point { radius = Pixels.float 2.5 }
        (Scene3d.Material.color entity.colour)
        entity.position


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
    | Orthographic Bool
    | ZoomChange Html.Events.Extra.Wheel.Event
      -- simulation stuff
    | NextD
    | HighlightPointsPos
    | HighlightPointsNeg
    | HighlightPointsInBoxRange
    | AddLeftBox
    | AddRightBox
    | AddCentreBox
    | ClearBox
    | DrawRhombiDocec Bool
    | DrawTetraHex Bool
    | DrawRhombiCube Bool
    | ToggleEdges Bool
    | ShowSubBox Bool
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
                            |> Quantity.clamp
                                (Angle.degrees -90)
                                (Angle.degrees 90)
                in
                ( { model
                    | azimuth = newAzimuth
                    , elevation = newElevation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        NextD ->
            case model.dimension of
                Dim1D ->
                    ( { model
                        | dimension = Dim2D
                        , points = newCoords model.points points2D
                      }
                    , Cmd.none
                    )

                Dim2D ->
                    ( { model
                        | dimension = Dim3D
                        , points = newCoords model.points points3D
                      }
                    , Cmd.none
                    )

                Dim3D ->
                    ( { model
                        | dimension = Dim1D
                        , points = newCoords model.points points1D
                      }
                    , Cmd.none
                    )

        Tick duration ->
            ( { model
                | points = List.map (animateEntity duration) model.points
              }
            , Cmd.none
            )

        ToTopDownCamera ->
            ( { model
                | azimuth = Angle.degrees 270
                , elevation = Angle.degrees 90
              }
            , Cmd.none
            )

        ToInitialCamera ->
            ( { model
                | azimuth = Angle.degrees 225
                , elevation = Angle.degrees 30
              }
            , Cmd.none
            )

        Orthographic b ->
            ( { model | orthographic = b }, Cmd.none )

        HighlightPointsPos ->
            ( { model
                | points =
                    List.map
                        (highlight ifPositive)
                        model.points
              }
            , Cmd.none
            )

        HighlightPointsNeg ->
            ( { model
                | points =
                    List.map
                        (highlight ifNegative)
                        model.points
              }
            , Cmd.none
            )

        HighlightPointsInBoxRange ->
            ( { model
                | points =
                    List.map
                        (highlight (ifInRangeofBox model.range model.box))
                        model.points
              }
            , Cmd.none
            )

        AddLeftBox ->
            ( { model
                | box =
                    Just <|
                        createBoxHelper 64 ( -64, -32, -32 )
              }
            , Cmd.none
            )

        AddRightBox ->
            ( { model
                | box =
                    Just <|
                        createBoxHelper 16 ( 0, 0, 0 )
              }
            , Cmd.none
            )

        AddCentreBox ->
            ( { model
                | box =
                    Just <|
                        createBoxHelper 1 ( 0, 0, 0 )
              }
            , Cmd.none
            )

        ClearBox ->
            ( { model | box = Nothing }, Cmd.none )

        ToggleEdges b ->
            ( { model | colourEdges = b }, Cmd.none )

        ShowSubBox b ->
            let
                sub =
                    case ( b, model.box ) of
                        ( True, Just box ) ->
                            createSubBoxes model.dimension box

                        _ ->
                            []
            in
            ( { model | showSubBox = b, subBoxes = sub }, Cmd.none )

        DrawRhombiDocec b ->
            ( { model | showRhombiDodec = b }, Cmd.none )

        DrawTetraHex b ->
            ( { model | showTetrakis = b }, Cmd.none )

        DrawRhombiCube b ->
            ( { model | showRhombiCube = b }, Cmd.none )

        ZoomChange event ->
            ( { model
                | distance =
                    Quantity.plus
                        model.distance
                        (Length.meters <| event.deltaY / 10)
                        |> Quantity.clamp
                            (Length.meters 50)
                            (Length.meters 250)
              }
            , Cmd.none
            )


createBoxHelper :
    Float
    -> ( Float, Float, Float )
    -> Block3d.Block3d Length.Meters Coords
createBoxHelper width centrePoint =
    let
        w =
            Length.meters width

        centre =
            case centrePoint of
                ( x, y, z ) ->
                    ( Length.meters x, Length.meters y, Length.meters z )
    in
    createBox w centre


createBox :
    Quantity Float Length.Meters
    ->
        ( Quantity Float Length.Meters
        , Quantity Float Length.Meters
        , Quantity Float Length.Meters
        )
    -> Block3d.Block3d Length.Meters Coords
createBox width centrePoint =
    let
        block r ( fromx, fromy, fromz ) =
            Block3d.from
                (Point3d.xyz
                    (Quantity.minus r fromx)
                    (Quantity.minus r fromy)
                    (Quantity.minus r fromz)
                )
                (Point3d.xyz
                    (Quantity.plus r fromx)
                    (Quantity.plus r fromy)
                    (Quantity.plus r fromz)
                )
    in
    block (Quantity.half width) centrePoint


createSubBoxes :
    Dimension
    -> Block3d.Block3d Length.Meters Coords
    -> List (Block3d.Block3d Length.Meters Coords)
createSubBoxes dim parentBox =
    let
        newWidth =
            case Block3d.dimensions parentBox of
                ( _, w, _ ) ->
                    Quantity.half w

        moveDistance =
            Quantity.half newWidth

        xAxis =
            Block3d.xAxis parentBox

        splitAlongX =
            List.map Point3d.coordinates
                [ Point3d.along xAxis moveDistance
                , Point3d.along xAxis (Quantity.negate moveDistance)
                ]
                |> List.map (createBox newWidth)
    in
    case dim of
        Dim1D ->
            splitAlongX

        Dim2D ->
            let
                yDirection =
                    Block3d.yAxis parentBox
                        |> Axis3d.direction

                doubleThenTranslate box =
                    [ Block3d.translateIn yDirection moveDistance box
                    , Block3d.translateIn
                        (Direction3d.reverse yDirection)
                        moveDistance
                        box
                    ]
            in
            List.map doubleThenTranslate splitAlongX
                |> List.concat

        Dim3D ->
            Block3d.vertices parentBox
                |> List.map (Block3d.from (Block3d.centerPoint parentBox))


animateEntity : Duration -> AnimatedPoint -> AnimatedPoint
animateEntity duration entity =
    case entity.futurePositions of
        currentTarget :: futurePositions ->
            let
                difference =
                    LineSegment3d.from entity.position currentTarget.position

                direction : Maybe (Direction3d.Direction3d Coords)
                direction =
                    LineSegment3d.direction difference
            in
            if Length.inMeters (LineSegment3d.length difference) < 0.5 then
                { entity
                    | position = currentTarget.position
                    , futurePositions = futurePositions
                }

            else
                case direction of
                    Just dir ->
                        let
                            speed =
                                (10 * currentTarget.speedMult)
                                    |> Speed.metersPerSecond
                                    |> Quantity.clamp
                                        (Speed.metersPerSecond 0)
                                        (Speed.metersPerSecond 100)

                            length : Length.Length
                            length =
                                Quantity.at speed duration

                            newFrom =
                                Point3d.translateBy
                                    (Vector3d.withLength length dir)
                                    entity.position
                        in
                        { entity
                            | position = newFrom
                            , futurePositions = entity.futurePositions
                        }

                    Nothing ->
                        entity

        _ ->
            entity


newCoords :
    List AnimatedPoint
    -> List (Point3d.Point3d Length.Meters Coords)
    -> List AnimatedPoint
newCoords entities newPoints =
    let
        setNewCoords :
            AnimatedPoint
            -> Point3d.Point3d Length.Meters Coords
            -> AnimatedPoint
        setNewCoords entity newPoint =
            let
                interpolated =
                    Point3d.interpolateFrom entity.position newPoint
            in
            { entity
                | futurePositions =
                    List.map2 Target
                        (List.map interpolated [ 0.6, 0.9, 0.95, 1 ])
                        [ 4, 2, 1.5, 1 ]
            }
    in
    List.map2 setNewCoords entities newPoints


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
            ++ (if List.any needsAnimated model.points then
                    [ Browser.Events.onAnimationFrameDelta
                        (Duration.milliseconds >> Tick)
                    ]

                else
                    []
               )


needsAnimated : AnimatedPoint -> Bool
needsAnimated entity =
    List.length entity.futurePositions > 0


colourLikeAxis : LineSegment3d.LineSegment3d units coordinates -> Color.Color
colourLikeAxis edge =
    case LineSegment3d.direction edge of
        Just dir ->
            if Direction3d.xComponent dir /= 0 then
                Color.red

            else if Direction3d.yComponent dir /= 0 then
                Color.green

            else if Direction3d.zComponent dir /= 0 then
                Color.blue

            else
                Color.white

        Nothing ->
            Color.white


view : Model -> Html Msg
view model =
    let
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0.5 0.5 0
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = model.distance
                }

        axes =
            case model.dimension of
                Dim1D ->
                    [ model.axes.x ]

                Dim2D ->
                    [ model.axes.x, model.axes.y ]

                Dim3D ->
                    [ model.axes.x, model.axes.y, model.axes.z ]

        createEdges box =
            let
                createEdge lineEdge =
                    let
                        edgeColour =
                            if model.colourEdges then
                                colourLikeAxis lineEdge

                            else
                                Color.white
                    in
                    Scene3d.lineSegment
                        (Scene3d.Material.color edgeColour)
                        lineEdge
            in
            Block3d.edges box
                |> List.map createEdge

        boundingBox =
            case model.box of
                Just box ->
                    createEdges box

                Nothing ->
                    []

        showRhombiDodec =
            case model.box of
                Just box ->
                    if model.showRhombiDodec then
                        Block3d.vertices box
                            |> List.map
                                (drawRhombiDodecEdges (Block3d.centerPoint box))
                            |> List.concat

                    else
                        []

                Nothing ->
                    []

        showTetraHex =
            case model.box of
                Just box ->
                    if model.showTetrakis then
                        drawTetraHexEdges model.range box

                    else
                        []

                Nothing ->
                    []

        showRhombiCube =
            case model.box of
                Just box ->
                    if model.showRhombiCube then
                        drawRhombiCubeEdges model.range box

                    else
                        []

                Nothing ->
                    []

        showSubBox =
            if model.showSubBox then
                List.map createEdges model.subBoxes
                    |> List.concat

            else
                []
    in
    main_
        [ style "box-sizing" "border-box"
        , style "width" "100%"
        , style "min-height" "100vh"
        , style "padding" "1rem"
        , style "display" "flex"
        , style "gap" "1rem"
        , style "flex-wrap" "wrap"
        , style "align-items" "start"
        , style "background" "dimgrey"
        ]
        [ section
            [ Html.Events.onMouseDown MouseDown
            , Html.Events.Extra.Wheel.onWheel ZoomChange
            , style "border" "2px solid black"
            , style "background" "slategrey"
            , style "cursor" "move"
            ]
            [ Scene3d.unlit
                { camera =
                    if model.orthographic then
                        Camera3d.orthographic
                            { viewpoint = viewpoint
                            , viewportHeight = Quantity.half model.distance
                            }

                    else
                        Camera3d.perspective
                            { viewpoint = viewpoint
                            , verticalFieldOfView = Angle.degrees 30
                            }
                , clipDepth = Length.meters 0.1
                , dimensions = ( Pixels.int 800, Pixels.int 600 )
                , background = Scene3d.transparentBackground
                , entities =
                    axes
                        ++ boundingBox
                        ++ List.map createSceneEntity model.points
                        ++ showRhombiDodec
                        ++ showTetraHex
                        ++ showRhombiCube
                        ++ showSubBox
                }
            ]
        , section [ style "flex" "1" ] <|
            [ p [] [ text "dimension" ]
            , button [ onClick NextD ] [ text "next dim" ]
            , p [] [ text "camera" ]
            , button [ onClick ToTopDownCamera ] [ text "top down" ]
            , button [ onClick ToInitialCamera ] [ text "fourtyfive" ]
            , div [ style "display" "flex", style "flex-direction" "column" ]
                [ label []
                    [ input
                        [ type_ "checkbox"
                        , onClick <| Orthographic (not model.orthographic)
                        , checked model.orthographic
                        ]
                        []
                    , text "toggle orthographic"
                    ]
                ]
            , p [] [ text "highlight" ]
            , button
                [ onClick HighlightPointsPos ]
                [ text "make positive x yellow" ]
            , button
                [ onClick HighlightPointsNeg ]
                [ text "make negative x yellow" ]
            , button
                [ onClick HighlightPointsInBoxRange ]
                [ text "make points in range yellow" ]
            , p [] [ text "bounding box" ]
            , button [ onClick AddLeftBox ] [ text "add big example box" ]
            , button [ onClick AddRightBox ] [ text "add example box" ]
            , button [ onClick AddCentreBox ] [ text "add wee example box" ]
            ]
                ++ (if model.box /= Nothing then
                        [ br [] []
                        , button
                            [ onClick <| ToggleEdges (not model.colourEdges) ]
                            [ text "colour box edges" ]
                        , button [ onClick ClearBox ] [ text "clear box" ]
                        , button
                            [ onClick <| ShowSubBox (not model.showSubBox) ]
                            [ text "show sub box" ]
                        , div
                            [ style "display" "flex"
                            , style "flex-direction" "column"
                            , style "user-select" "none"
                            ]
                            [ label [ style "color" "orange" ]
                                [ input
                                    [ type_ "checkbox"
                                    , onClick <|
                                        DrawRhombiDocec
                                            (not model.showRhombiDodec)
                                    , checked model.showRhombiDodec
                                    ]
                                    []
                                , text "draw rhombic dodecahedron"
                                ]
                            , label [ style "color" "yellow" ]
                                [ input
                                    [ type_ "checkbox"
                                    , onClick <|
                                        DrawTetraHex (not model.showTetrakis)
                                    , checked model.showTetrakis
                                    ]
                                    []
                                , text "draw tetrakis hexahahedron"
                                ]
                            , label [ style "color" "purple" ]
                                [ input
                                    [ type_ "checkbox"
                                    , onClick <|
                                        DrawRhombiCube
                                            (not model.showRhombiCube)
                                    , checked model.showRhombiCube
                                    ]
                                    []
                                , text "draw rhombicubeoctahedron"
                                ]
                            ]
                        ]

                    else
                        []
                   )
        ]


{-| Rhombic Dodecahedron <https://en.wikipedia.org/wiki/Rhombic_dodecahedron>
-}
drawRhombiDodecEdges :
    Point3d.Point3d Length.Meters Coords
    -> Point3d.Point3d Length.Meters Coords
    -> List (Scene3d.Entity Coords)
drawRhombiDodecEdges centrePoint cubeVertex =
    let
        axisThroughCentre =
            Axis3d.throughPoints cubeVertex centrePoint

        verticalAxis =
            Axis3d.through cubeVertex Direction3d.x

        createEdge axis from to rotate =
            LineSegment3d.from from to
                |> LineSegment3d.rotateAround verticalAxis (Angle.degrees 90)
                |> LineSegment3d.rotateAround axis (Angle.degrees rotate)
                |> Scene3d.lineSegment (Scene3d.Material.color Color.orange)
    in
    case axisThroughCentre of
        Just axis ->
            List.map (createEdge axis cubeVertex centrePoint) [ 0, 120, 240 ]

        Nothing ->
            []


{-| Tetrakis Hexahedron <https://en.wikipedia.org/wiki/Tetrakis_hexahedron>
-}
drawTetraHexEdges :
    Float
    -> Block3d.Block3d Length.Meters Coords
    -> List (Scene3d.Entity Coords)
drawTetraHexEdges range box =
    let
        centrePoint =
            Block3d.centerPoint box

        createAxis =
            Axis3d.through centrePoint

        xAxis =
            createAxis Direction3d.x

        yAxis =
            createAxis Direction3d.y

        zAxis =
            createAxis Direction3d.z

        width =
            case Block3d.dimensions box of
                ( _, w, _ ) ->
                    w

        distance =
            Quantity.plus (Quantity.half width) (Length.meters range)

        points =
            [ Point3d.along xAxis distance
            , Point3d.along xAxis (Quantity.negate distance)
            , Point3d.along yAxis distance
            , Point3d.along yAxis (Quantity.negate distance)
            , Point3d.along zAxis distance
            , Point3d.along zAxis (Quantity.negate distance)
            ]

        createLineSegment from to =
            LineSegment3d.from from to

        isShortSegment segment =
            LineSegment3d.length segment
                |> Quantity.lessThan (Quantity.plus width (Length.meters range))

        createEdges point =
            List.map (createLineSegment point) (Block3d.vertices box)
                |> List.filter isShortSegment
                |> List.map
                    (Scene3d.lineSegment (Scene3d.Material.color Color.yellow))
    in
    List.map createEdges points
        |> List.concat


{-| Rhombicuboctahedron <https://en.wikipedia.org/wiki/Rhombicuboctahedron>
-}
drawRhombiCubeEdges :
    Float
    -> Block3d.Block3d Length.Meters Coords
    -> List (Scene3d.Entity Coords)
drawRhombiCubeEdges range box =
    let
        centrePoint =
            Block3d.centerPoint box

        xAxis v =
            Axis3d.through v Direction3d.x

        yAxis v =
            Axis3d.through v Direction3d.y

        zAxis v =
            Axis3d.through v Direction3d.z

        distance =
            Length.meters range

        createVertices vertex =
            [ Point3d.along (xAxis vertex) distance
            , Point3d.along (xAxis vertex) (Quantity.negate distance)
            , Point3d.along (yAxis vertex) distance
            , Point3d.along (yAxis vertex) (Quantity.negate distance)
            , Point3d.along (zAxis vertex) distance
            , Point3d.along (zAxis vertex) (Quantity.negate distance)
            ]

        allVertices =
            List.map createVertices (Block3d.vertices box)
                |> List.concat

        findMaxDistance vertex max =
            let
                d =
                    Point3d.distanceFrom vertex centrePoint
            in
            if Quantity.greaterThan max d then
                d

            else
                max

        outerVertices =
            List.filter isOuterVertex allVertices

        isOuterVertex vertex =
            Point3d.distanceFrom vertex centrePoint
                |> Quantity.equalWithin (Length.meters 0.1) maxDist

        maxDist =
            List.foldl findMaxDistance (Length.meters 0) allVertices

        buildEdges vertices vertex =
            List.map (createLine vertex) vertices
                |> List.filter isShortSegment
                |> List.map
                    (Scene3d.lineSegment
                        (Scene3d.Material.color Color.purple)
                    )

        createLine from to =
            LineSegment3d.from from to

        isShortSegment lineSegment =
            let
                length =
                    LineSegment3d.length lineSegment

                lengthSquared =
                    Quantity.squared length

                -- outer face edges have same length as inner cube
                isOuterFaceEdge =
                    Quantity.equalWithin (Length.meters 0.1) width length

                -- connecting diagonal edges have length of sqrt(2r^2)
                -- compare to length^2 to skip doing the sqrt
                isConnectingDiagEdge =
                    Quantity.squared (Length.meters range)
                        |> Quantity.multiplyBy 2
                        |> Quantity.equalWithin
                            (Quantity.squared (Length.meters 0.1))
                            lengthSquared
            in
            isOuterFaceEdge || isConnectingDiagEdge

        width =
            case Block3d.dimensions box of
                ( _, w, _ ) ->
                    w
    in
    List.map (buildEdges outerVertices) outerVertices
        |> List.concat


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
