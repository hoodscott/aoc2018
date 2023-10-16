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
import Html.Attributes exposing (checked, class, classList, for, id, name, type_)
import Html.Events exposing (onClick)
import Html.Events.Extra.Wheel
import Html.Keyed
import Json.Decode as Decode exposing (Decoder)
import Length
import LineSegment3d
import Pixels exposing (Pixels)
import Point3d
import PriorityQueue exposing (PriorityQueue)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material
import Speed
import String
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



-- TYPES


type Coords
    = Coords


type Projection
    = Perspective
    | Orthographic


type Dimension
    = Dim1D
    | Dim2D
    | Dim3D


type SimulationState
    = Uninitialised
    | Start
    | SplitIntoSubBoxes Int
    | ScoredBox Int
    | Solved (Point3d.Point3d Length.Meters Coords) Int


type SimulationControl
    = Manual
    | FastForward
    | Skip


type alias Target =
    { position : Point3d.Point3d Length.Meters Coords
    , speedMult : Float
    }


type alias AnimatedPoint =
    { position : Point3d.Point3d Length.Meters Coords
    , futurePositions : List Target
    , highlighted : Bool
    }


type alias Model =
    { -- camera
      azimuth : Angle.Angle -- orbit angle of the camera around the focal point
    , elevation : Angle.Angle -- angle of the camera up from the XY plane
    , distance : Length.Length -- distance camera is from origin
    , orbiting : Bool -- are we moving the camera
    , projection : Projection -- camera projection

    -- problem simulation
    , state : SimulationState
    , control : SimulationControl
    , dimension : Dimension -- how many dimensions are we showing
    , range : Float
    , candidates : PriorityQueue (Block3d.Block3d Length.Meters Coords) Float
    , toBeScored : List (Block3d.Block3d Length.Meters Coords)

    -- simulation visuals (scene3d elements)
    , points : List AnimatedPoint
    , simSelectedBox : Maybe (Block3d.Block3d Length.Meters Coords)
    , simSubBoxes : List (Block3d.Block3d Length.Meters Coords)
    , userSelectedBox : Maybe (Block3d.Block3d Length.Meters Coords)
    , axes :
        { x : Scene3d.Entity Coords
        , y : Scene3d.Entity Coords
        , z : Scene3d.Entity Coords
        }

    -- extra options toggles
    , showRhombiCube : Bool
    , showRhombiDodec : Bool
    , showTetrakis : Bool
    , showRealPriority : Bool
    }



-- INIT


initialModel : Model
initialModel =
    { --camera
      azimuth = Angle.degrees 225
    , elevation = Angle.degrees 30
    , distance = Length.meters 200
    , orbiting = False
    , projection = Orthographic

    -- problem simulation
    , state = Uninitialised
    , control = Manual
    , dimension = Dim3D
    , range = 4
    , candidates = []
    , toBeScored = []

    -- simulation visuals (scene3d elements)
    , points = List.map createAnimatedEntity points3D
    , simSelectedBox = Nothing
    , simSubBoxes = []
    , userSelectedBox = Nothing
    , axes =
        let
            axisSize =
                65
        in
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

    -- extra options toggles
    , showRhombiCube = False
    , showRhombiDodec = False
    , showTetrakis = False
    , showRealPriority = False
    }


createAnimatedEntity : Point3d.Point3d Length.Meters Coords -> AnimatedPoint
createAnimatedEntity point =
    { position = point, futurePositions = [], highlighted = False }


init : () -> ( Model, Cmd msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = -- camera control
      ToTopDownCamera
    | ToInitialCamera
    | ToggleProjection
    | ZoomChange Html.Events.Extra.Wheel.Event
      -- mouse events
    | MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
      -- simulation controls
    | StartSim
    | StepSim
    | NextD
    | SetControl SimulationControl
    | HighlightBox (Maybe (Block3d.Block3d Length.Meters Coords))
      -- optional toggles
    | DrawRhombiCube Bool
    | DrawRhombiDocec Bool
    | DrawTetraHex Bool
    | TogglePrecision Bool
      -- animation ticks
    | Tick Duration


update : Msg -> Model -> ( Model, Cmd Msg )
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
            let
                newModel =
                    case model.dimension of
                        Dim1D ->
                            { model
                                | dimension = Dim2D
                                , points = newCoords model.points points2D
                            }

                        Dim2D ->
                            { model
                                | dimension = Dim3D
                                , points = newCoords model.points points3D
                            }

                        Dim3D ->
                            { model
                                | dimension = Dim1D
                                , points = newCoords model.points points1D
                            }
            in
            update StartSim newModel

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

        ToggleProjection ->
            let
                newProjection =
                    case model.projection of
                        Orthographic ->
                            Perspective

                        Perspective ->
                            Orthographic
            in
            ( { model | projection = newProjection }, Cmd.none )

        DrawRhombiDocec b ->
            ( { model | showRhombiDodec = b }, Cmd.none )

        DrawTetraHex b ->
            ( { model | showTetrakis = b }, Cmd.none )

        DrawRhombiCube b ->
            ( { model | showRhombiCube = b }, Cmd.none )

        TogglePrecision b ->
            ( { model | showRealPriority = b }, Cmd.none )

        SetControl c ->
            let
                newModel =
                    { model | control = c }
            in
            if c == Skip then
                simulateStep newModel

            else
                ( newModel, Cmd.none )

        HighlightBox box ->
            ( { model | userSelectedBox = box }, Cmd.none )

        ZoomChange event ->
            ( { model
                | distance =
                    Quantity.plus
                        model.distance
                        (Length.meters <| event.deltaY / 10)
                        |> Quantity.clamp
                            (Length.meters 50)
                            (Length.meters 350)
              }
            , Cmd.none
            )

        StartSim ->
            let
                initialBox =
                    createBoxFromFloat 128
                        -- dimensions we are searching for need to be offset
                        -- by half unit to ensure solution is an integer
                        (case model.dimension of
                            Dim1D ->
                                ( -0.5, 0, 0 )

                            Dim2D ->
                                ( -0.5, -0.5, 0 )

                            Dim3D ->
                                ( -0.5, -0.5, -0.5 )
                        )

                initialPoints =
                    List.map (highlight (\_ -> False))
                        model.points
            in
            ( { model
                | state = Start
                , simSelectedBox = Nothing
                , userSelectedBox = Nothing
                , points = initialPoints
                , control = Manual
                , candidates =
                    PriorityQueue.insert
                        (toFloat <| List.length model.points)
                        initialBox
                        []
                , toBeScored = []
                , simSubBoxes = []
              }
            , Cmd.none
            )

        StepSim ->
            simulateStep model


ifInRangeofBox :
    Float
    -> Block3d.Block3d Length.Meters Coords
    -> AnimatedPoint
    -> Bool
ifInRangeofBox range box entity =
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


highlight : (AnimatedPoint -> Bool) -> AnimatedPoint -> AnimatedPoint
highlight condition entity =
    { entity | highlighted = condition entity }


createBoxFromFloat :
    Float
    -> ( Float, Float, Float )
    -> Block3d.Block3d Length.Meters Coords
createBoxFromFloat width centrePoint =
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


simulateStep : Model -> ( Model, Cmd msg )
simulateStep model =
    -- step only works until we find the answer
    case model.state of
        Solved _ _ ->
            ( model, Cmd.none )

        _ ->
            case model.toBeScored of
                -- need to score all boxes before splitting any more
                boxtoScore :: tail ->
                    scoreBox model boxtoScore tail

                _ ->
                    -- no boxes to be scored so try to pop a new
                    -- candidate, check to see if it is a solution or
                    -- split into more subboxes
                    case PriorityQueue.pop model.candidates of
                        ( Just poppedBox, newCandidates ) ->
                            let
                                width =
                                    case Block3d.dimensions poppedBox of
                                        ( _, w, _ ) ->
                                            w
                            in
                            if
                                Quantity.equalWithin
                                    (Length.meters 0.1)
                                    (Length.meters 1)
                                    width
                            then
                                foundSolution model poppedBox newCandidates

                            else
                                splitCandidate model poppedBox newCandidates

                        _ ->
                            -- Priority queue is empty
                            -- (should have already found solution)
                            ( model, Cmd.none )


scoreBox :
    Model
    -> Block3d.Block3d Length.Meters Coords
    -> List (Block3d.Block3d Length.Meters Coords)
    -> ( Model, Cmd msg )
scoreBox model boxtoScore tail =
    let
        newPoints =
            List.map
                (highlight (ifInRangeofBox model.range boxtoScore))
                model.points

        score =
            calcScore newPoints

        smallBoxBoost =
            case Block3d.dimensions boxtoScore of
                ( _, w, _ ) ->
                    Length.inMeters w
                        |> (*) 0.001
                        |> (-) 1

        newModel =
            { model
                | state = ScoredBox score
                , points = newPoints
                , toBeScored = tail
                , candidates =
                    PriorityQueue.insert
                        (score + smallBoxBoost)
                        boxtoScore
                        model.candidates
                , simSelectedBox = Just boxtoScore
                , simSubBoxes = []
            }
    in
    if model.control == Skip then
        simulateStep newModel

    else
        ( newModel, Cmd.none )


calcScore : List AnimatedPoint -> number
calcScore points =
    let
        countHighlighted point acc =
            if point.highlighted then
                acc + 1

            else
                acc
    in
    List.foldl countHighlighted 0 points


foundSolution :
    Model
    -> Block3d.Block3d Length.Meters Coords
    -> PriorityQueue (Block3d.Block3d Length.Meters Coords) Float
    -> ( Model, Cmd msg )
foundSolution model poppedBox newCandidates =
    let
        newPoints =
            List.map
                (highlight
                    (ifInRangeofBox model.range poppedBox)
                )
                model.points

        score =
            calcScore newPoints

        newModel =
            { model
                | state =
                    Solved
                        (Block3d.centerPoint poppedBox)
                        score
                , candidates = newCandidates
                , points = newPoints
                , simSubBoxes = []
                , simSelectedBox = Just poppedBox
            }
    in
    if model.control == Skip then
        simulateStep newModel

    else
        ( newModel, Cmd.none )


splitCandidate :
    Model
    -> Block3d.Block3d Length.Meters Coords
    -> PriorityQueue (Block3d.Block3d Length.Meters Coords) Float
    -> ( Model, Cmd msg )
splitCandidate model poppedBox newCandidates =
    let
        subBoxes =
            createSubBoxes model.dimension poppedBox

        pointsNoHighlight =
            List.map (highlight (\_ -> False))
                model.points

        widthInt =
            case Block3d.dimensions poppedBox of
                ( _, w, _ ) ->
                    w |> Length.inMeters |> round

        newModel =
            { model
                | state = SplitIntoSubBoxes widthInt
                , candidates = newCandidates
                , toBeScored = List.reverse subBoxes
                , points = pointsNoHighlight
                , simSubBoxes = subBoxes
                , simSelectedBox = Just poppedBox
            }
    in
    if model.control == Skip then
        simulateStep newModel

    else
        ( newModel, Cmd.none )



-- SUBSCRIPTIONS


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
            ++ (if
                    simulationInProgress model.state
                        && model.control
                        == FastForward
                then
                    [ Browser.Events.onAnimationFrame (always StepSim) ]

                else
                    []
               )


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


needsAnimated : AnimatedPoint -> Bool
needsAnimated entity =
    List.length entity.futurePositions > 0


simulationInProgress : SimulationState -> Bool
simulationInProgress state =
    case state of
        Solved _ _ ->
            False

        Uninitialised ->
            False

        Start ->
            True

        SplitIntoSubBoxes _ ->
            True

        ScoredBox _ ->
            True



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ viewSectionScene model
        , viewSectionControls model
        , viewSectionCandidatesQueue
            model.showRealPriority
            model.dimension
            model.userSelectedBox
            model.candidates
        , viewSectionToScoreQueue
            model.dimension
            model.userSelectedBox
            model.toBeScored
        ]


viewSectionScene : Model -> Html Msg
viewSectionScene model =
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

        createEdges colour box =
            let
                createEdge lineEdge =
                    Scene3d.lineSegment
                        (Scene3d.Material.color colour)
                        lineEdge
            in
            Block3d.edges box
                |> List.map createEdge

        boundingBox =
            case model.simSelectedBox of
                Just box ->
                    createEdges Color.white box

                Nothing ->
                    []

        boundingBoxUser =
            case model.userSelectedBox of
                Just box ->
                    createEdges Color.red box

                Nothing ->
                    []

        showRhombiDodec =
            case model.simSelectedBox of
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
            case model.simSelectedBox of
                Just box ->
                    if model.showTetrakis then
                        drawTetraHexEdges model.range box

                    else
                        []

                Nothing ->
                    []

        showRhombiCube =
            case model.simSelectedBox of
                Just box ->
                    if model.showRhombiCube then
                        drawRhombiCubeEdges model.range box

                    else
                        []

                Nothing ->
                    []

        showSubBox =
            List.map (createEdges Color.white) model.simSubBoxes
                |> List.concat
    in
    section
        [ Html.Events.onMouseDown MouseDown
        , Html.Events.Extra.Wheel.onWheel ZoomChange
        , class "scene-window"
        ]
        [ Scene3d.unlit
            { camera =
                case model.projection of
                    Orthographic ->
                        Camera3d.orthographic
                            { viewpoint = viewpoint
                            , viewportHeight = Quantity.half model.distance
                            }

                    Perspective ->
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
                    ++ boundingBoxUser
                    ++ List.map createSceneEntity model.points
                    ++ showRhombiDodec
                    ++ showTetraHex
                    ++ showRhombiCube
                    ++ showSubBox
            }
        ]


viewSectionControls : Model -> Html Msg
viewSectionControls model =
    let
        pluralise singular plural number =
            if number == 1 then
                singular

            else
                plural

        showButtons =
            [ p [] [ text "simulation" ]
            , Html.Keyed.node "div"
                []
                [ case model.state of
                    Solved _ _ ->
                        ( "solved"
                        , div []
                            [ button [ onClick StartSim ] [ text "start again" ]
                            ]
                        )

                    Uninitialised ->
                        ( "unit"
                        , div []
                            [ button [ onClick StartSim ] [ text "start" ] ]
                        )

                    _ ->
                        case model.control of
                            FastForward ->
                                ( "fastforwarding"
                                , div []
                                    [ button
                                        [ onClick <| SetControl Manual ]
                                        [ text "stop fast forward" ]
                                    , button
                                        [ onClick <| SetControl Skip ]
                                        [ text "skip to end" ]
                                    ]
                                )

                            Manual ->
                                ( "stepper"
                                , div []
                                    [ button
                                        [ onClick StepSim ]
                                        [ text "step" ]
                                    , button
                                        [ onClick <| SetControl FastForward ]
                                        [ text "fast forward" ]
                                    , button
                                        [ onClick <| SetControl Skip ]
                                        [ text "skip to end" ]
                                    , button
                                        [ onClick StartSim ]
                                        [ text "reset" ]
                                    ]
                                )

                            Skip ->
                                ( "skipping", div [] [ text "skipping..." ] )
                ]
            ]
                ++ viewPseudoCode model
                ++ (case model.state of
                        Solved answer score ->
                            [ p []
                                [ text <|
                                    "Box has a width of 1 so the answer is: "
                                , viewAnswer model.dimension answer
                                , text <| " with " ++ String.fromInt score ++ " " ++ pluralise "point" "points" score ++ " in range."
                                ]
                            ]

                        Uninitialised ->
                            []

                        Start ->
                            [ p [] [ text "Initial box added." ] ]

                        SplitIntoSubBoxes width ->
                            [ p []
                                [ text <|
                                    "Box has a width of "
                                        ++ String.fromInt width
                                        ++ ", so split into smaller boxes."
                                ]
                            ]

                        ScoredBox score ->
                            [ p []
                                [ text <|
                                    "Box has "
                                        ++ String.fromInt score
                                        ++ " "
                                        ++ pluralise "point" "points" score
                                        ++ " in range.  Added to queue."
                                ]
                            ]
                   )
    in
    section [] <|
        [ p [] [ text "dimension" ]
        , button [ onClick NextD ] [ text "next dim" ]
        , p [] [ text "camera" ]
        , button [ onClick ToTopDownCamera ] [ text "top down" ]
        , button [ onClick ToInitialCamera ] [ text "fourtyfive" ]
        , div []
            [ label [ for "toggle-projection" ]
                [ input
                    [ type_ "checkbox"
                    , onClick <| ToggleProjection
                    , checked (model.projection == Orthographic)
                    , id "toggle-projection"
                    ]
                    []
                , text "toggle orthographic"
                ]
            ]
        ]
            ++ showButtons
            ++ (if model.simSelectedBox /= Nothing || model.userSelectedBox /= Nothing then
                    [ p [] [ text "show extra shapes" ]
                    , div []
                        [ label
                            [ for "show-rhombicube" ]
                            [ input
                                [ type_ "checkbox"
                                , onClick <|
                                    DrawRhombiCube
                                        (not model.showRhombiCube)
                                , checked model.showRhombiCube
                                , id "show-rhombicube"
                                ]
                                []
                            , text "draw rhombicubeoctahedron"
                            ]
                        , label
                            [ for "show-rhombidec" ]
                            [ input
                                [ type_ "checkbox"
                                , onClick <|
                                    DrawRhombiDocec
                                        (not model.showRhombiDodec)
                                , checked model.showRhombiDodec
                                , id "show-rhombidec"
                                ]
                                []
                            , text "draw rhombic dodecahedron"
                            ]
                        , label
                            [ for "show-tetrakis" ]
                            [ input
                                [ type_ "checkbox"
                                , onClick <|
                                    DrawTetraHex (not model.showTetrakis)
                                , checked model.showTetrakis
                                , id "show-tetrakis"
                                ]
                                []
                            , text "draw tetrakis hexahahedron"
                            ]
                        ]
                    , p [] [ text "misc options" ]
                    , div []
                        [ label
                            [ for "toggle-precision" ]
                            [ input
                                [ type_ "checkbox"
                                , onClick <|
                                    TogglePrecision
                                        (not model.showRealPriority)
                                , checked model.showRealPriority
                                , id "toggle-precision"
                                ]
                                []
                            , text "toggle precision"
                            ]
                        ]
                    ]
                        ++ (if model.userSelectedBox /= Nothing then
                                [ br [] []
                                , button [ onClick <| HighlightBox Nothing ]
                                    [ text "clear selectedbox"
                                    ]
                                ]

                            else
                                []
                           )

                else
                    []
               )


viewPseudoCode : Model -> List (Html msg)
viewPseudoCode model =
    let
        start =
            model.state == Start

        solved =
            case model.state of
                Solved _ _ ->
                    True

                _ ->
                    False

        split =
            case model.state of
                SplitIntoSubBoxes _ ->
                    True

                _ ->
                    False

        score =
            case model.state of
                ScoredBox _ ->
                    True

                _ ->
                    False
    in
    [ div [ class "code-container" ]
        [ code [ classList [ ( "active", start ) ] ]
            [ text "add box containing all points to priority queue" ]
        , code [ classList [ ( "active", solved || split ) ] ]
            [ text "while the priority queue is not empty:"
            , code []
                [ text "take the first box from the queue" ]
            , code [ classList [ ( "active", solved ), ( "skipped", split ) ] ]
                [ text "if the box's width is 1:"
                , code [] [ text "we have found a solution" ]
                ]
            , code [ classList [ ( "skipped", solved ) ] ]
                [ text "otherwise:"
                , code [ classList [ ( "active", split ) ] ]
                    [ text "split the box into smaller boxes" ]
                , code [ classList [ ( "active", score ), ( "skipped", split ) ] ]
                    [ text "for each smaller box: "
                    , code [] [ text "count how many points are in range" ]
                    , code []
                        [ text "add this box to the priority queue"
                        , br [] []
                        , text "with a priority of the calculated count"
                        ]
                    ]
                ]
            ]
        ]
    ]


viewSectionCandidatesQueue :
    Bool
    -> Dimension
    -> Maybe (Block3d.Block3d Length.Meters Coords)
    -> PriorityQueue (Block3d.Block3d Length.Meters Coords) Float
    -> Html Msg
viewSectionCandidatesQueue showRealPriority dimension selectedBlock candidates =
    section []
        [ table []
            [ caption []
                [ text <|
                    "Priority Queue - Total: "
                        ++ (String.fromInt <| List.length candidates)
                ]
            , thead []
                [ tr []
                    [ th [] [ text "Priority" ]
                    , th [] [ text "Width" ]
                    , th [] [ text "Centrepoint" ]
                    , th [] [ text "Highlight" ]
                    ]
                ]
            , tbody [] <|
                List.map
                    (showQueueItem showRealPriority dimension selectedBlock)
                    candidates
            ]
        ]


viewSectionToScoreQueue :
    Dimension
    -> Maybe (Block3d.Block3d Length.Meters Coords)
    -> List (Block3d.Block3d Length.Meters Coords)
    -> Html Msg
viewSectionToScoreQueue dimension selectedBlock toBeScored =
    section []
        [ table []
            [ caption []
                [ text <|
                    "Boxes to be Scored - Total: "
                        ++ (String.fromInt <| List.length toBeScored)
                ]
            , thead []
                [ tr []
                    [ th [] [ text "Width" ]
                    , th [] [ text "Centrepoint" ]
                    , th [] [ text "Highlight" ]
                    ]
                ]
            , tbody [] <|
                List.map (showScoreItem dimension selectedBlock) toBeScored
            ]
        ]


createSceneEntity : AnimatedPoint -> Scene3d.Entity Coords
createSceneEntity entity =
    let
        colour =
            if entity.highlighted then
                Color.yellow

            else
                Color.white
    in
    Scene3d.point { radius = Pixels.float 2.5 }
        (Scene3d.Material.color colour)
        entity.position


viewAnswer : Dimension -> Point3d.Point3d Length.Meters Coords -> Html msg
viewAnswer dimension point =
    text <| "(" ++ pointToCoordString dimension point ++ ")"


showQueueItem :
    Bool
    -> Dimension
    -> Maybe (Block3d.Block3d Length.Meters Coords)
    -> PriorityQueue.PQElement (Block3d.Block3d Length.Meters Coords) Float
    -> Html Msg
showQueueItem showRealPriority dimension selectedBlock pqElem =
    let
        priority =
            if showRealPriority then
                String.fromFloat (PriorityQueue.getPriority pqElem)

            else
                floor (PriorityQueue.getPriority pqElem)
                    |> String.fromInt
    in
    tr [] <|
        td [] [ text priority ]
            :: (viewBlock dimension selectedBlock <|
                    PriorityQueue.getElement pqElem
               )


showScoreItem :
    Dimension
    -> Maybe (Block3d.Block3d Length.Meters Coords)
    -> Block3d.Block3d Length.Meters Coords
    -> Html Msg
showScoreItem dimension selectedBlock boxToBeScored =
    tr [] <| viewBlock dimension selectedBlock boxToBeScored


viewBlock :
    Dimension
    -> Maybe (Block3d.Block3d Length.Meters Coords)
    -> Block3d.Block3d Length.Meters Coords
    -> List (Html Msg)
viewBlock dimension selectedBlock block =
    let
        centreString =
            pointToCoordString dimension <| Block3d.centerPoint block

        widthString =
            case Block3d.dimensions block of
                ( _, w, _ ) ->
                    w |> Length.inMeters |> String.fromFloat

        isActive =
            case selectedBlock of
                Just selected ->
                    selected == block

                Nothing ->
                    False

        inputID =
            String.filter Char.isDigit (widthString ++ centreString)
                |> (++) "highlight-"

        attrs =
            (if isActive then
                [ onClick <| HighlightBox Nothing ]

             else
                [ onClick <| HighlightBox (Just block) ]
            )
                ++ [ type_ "radio"
                   , name "highlight"
                   , checked isActive
                   , id inputID
                   ]
    in
    [ td [] [ text widthString ]
    , td [] [ text <| "(" ++ centreString ++ ")" ]
    , td []
        [ label [ for inputID ] [ input attrs [], text "select" ] ]
    ]


pointToCoordString :
    Dimension
    -> Point3d.Point3d Length.Meters Coords
    -> String
pointToCoordString dimension point =
    let
        flooredString float =
            floor float |> String.fromInt
    in
    case Point3d.toTuple Length.inMeters point of
        ( x, y, z ) ->
            case dimension of
                Dim1D ->
                    flooredString x

                Dim2D ->
                    flooredString x ++ ", " ++ flooredString y

                Dim3D ->
                    flooredString x
                        ++ ", "
                        ++ flooredString y
                        ++ ", "
                        ++ flooredString z
                        ++ ""


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



-- DATA


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
