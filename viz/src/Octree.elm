module Octree exposing (main)

import Angle
import Axis3d
import Block3d
import BoundingBox3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Char
import Data exposing (Coords, palette)
import Direction3d
import Duration exposing (Duration)
import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, disabled, for, id, name, tabindex, type_)
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
import Task
import Vector3d
import Viewpoint3d


main : Program Bool Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



-- TYPES


type Projection
    = Perspective
    | Orthographic


type Dimension
    = Dim1D
    | Dim2D
    | Dim3D


type State
    = Intro
    | Simulation Dimension SimulationState SimulationControl
    | Wrapup


type SimulationState
    = Start
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
    , visualPosition : Point3d.Point3d Length.Meters Coords
    , futurePositions : List Target
    , highlighted : Bool
    }


type alias Model =
    { -- camera
      camAzimuth : Angle.Angle -- orbit angle around the focal point
    , camElevation : Angle.Angle -- angle up from the XY plane
    , camDistance : Length.Length -- distance from origin
    , camOrbiting : Bool -- are we moving the camera
    , camMoved : Bool -- has camera move?
    , camProjection : Projection -- orthographic or perspective
    , camSize : Maybe (Quantity Int Pixels)

    -- problem simulation
    , state : State
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
    , showOptions : Bool
    , showRhombiCube : Bool
    , showRhombiDodec : Bool
    , showTetrakis : Bool
    , showRealPriority : Bool

    -- accessibility
    , prefersReducedMotion : Bool
    }



-- INIT


initialModel : Model
initialModel =
    { --camera
      camAzimuth = Angle.degrees 270
    , camElevation = Angle.degrees 90
    , camDistance = Length.meters 270
    , camOrbiting = False
    , camMoved = False
    , camProjection = Orthographic
    , camSize = Nothing

    -- problem simulation
    , state = Intro
    , range = 4
    , candidates = []
    , toBeScored = []

    -- simulation visuals (scene3d elements)
    , points = List.map createAnimatedEntity Data.points1D
    , simSelectedBox = Nothing
    , simSubBoxes = []
    , userSelectedBox = Nothing
    , axes =
        let
            axisSize =
                65
        in
        { x =
            Scene3d.lineSegment (Scene3d.Material.color palette.red) <|
                LineSegment3d.from
                    (Point3d.meters -axisSize 0 0)
                    (Point3d.meters axisSize 0 0)
        , y =
            Scene3d.lineSegment (Scene3d.Material.color palette.green) <|
                LineSegment3d.from
                    (Point3d.meters 0 -axisSize 0)
                    (Point3d.meters 0 axisSize 0)
        , z =
            Scene3d.lineSegment (Scene3d.Material.color palette.blue) <|
                LineSegment3d.from
                    (Point3d.meters 0 0 -axisSize)
                    (Point3d.meters 0 0 axisSize)
        }

    -- extra options toggles
    , showOptions = False
    , showRhombiCube = False
    , showRhombiDodec = False
    , showTetrakis = False
    , showRealPriority = False

    -- accessibility
    , prefersReducedMotion = False
    }


createAnimatedEntity : Point3d.Point3d Length.Meters Coords -> AnimatedPoint
createAnimatedEntity point =
    AnimatedPoint point point [] False


init : Bool -> ( Model, Cmd msg )
init flagPreferReducedMotion =
    ( { initialModel | prefersReducedMotion = flagPreferReducedMotion }
    , Cmd.none
    )



-- UPDATE


type Msg
    = -- mouse events
      DepressedMouse
    | ReleasedMouse
    | MovedMouse (Quantity Float Pixels) (Quantity Float Pixels)
      -- camera controls
    | SelectedTopDownCamera
    | SelectedInitialCamera
    | ToggledProjection
    | ChangedZoom Html.Events.Extra.Wheel.Event
    | ReturnedViewportSize (Result Browser.Dom.Error Browser.Dom.Element)
    | RequestedViewportSize
      -- simulation controls
    | ChangedState State
    | StartedSim
    | SteppedSim
    | SetControl SimulationControl
    | SelectedBox (Maybe (Block3d.Block3d Length.Meters Coords))
    | AttemptedFocus (Result Browser.Dom.Error ())
      -- optional toggles
    | ToggledPrecision Bool
    | ToggledRhombiCubeOption Bool
    | ToggledRhombiDodecOption Bool
    | ToggledTetraHexOption Bool
      -- animation ticks
    | TickedAnimation Duration


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- mouse events
        DepressedMouse ->
            ( { model | camOrbiting = True }, Cmd.none )

        ReleasedMouse ->
            ( { model | camOrbiting = False }, Cmd.none )

        MovedMouse dx dy ->
            if model.camOrbiting then
                let
                    -- orbit the camera by 1 degree per pixel of drag
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- adjust azimuth according to horizontal mouse motion
                    newAzimuth =
                        model.camAzimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- adjust elevation according to vertical mouse motion
                    newElevation =
                        model.camElevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            -- clamp to make sure camera cannot go past vertical
                            -- in either direction
                            |> Quantity.clamp
                                (Angle.degrees -90)
                                (Angle.degrees 90)
                in
                ( { model
                    | camAzimuth = newAzimuth
                    , camElevation = newElevation
                    , camMoved = True
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        -- camera controls
        SelectedTopDownCamera ->
            ( { model
                | camAzimuth = Angle.degrees 270
                , camElevation = Angle.degrees 90
              }
            , Cmd.none
            )

        SelectedInitialCamera ->
            ( { model
                | camAzimuth = Angle.degrees 225
                , camElevation = Angle.degrees 30
              }
            , Cmd.none
            )

        ToggledProjection ->
            let
                newProjection =
                    case model.camProjection of
                        Orthographic ->
                            Perspective

                        Perspective ->
                            Orthographic
            in
            ( { model | camProjection = newProjection }, Cmd.none )

        ChangedZoom event ->
            ( { model
                | camDistance =
                    Quantity.plus
                        model.camDistance
                        (Length.meters <| event.deltaY / 10)
                        |> Quantity.clamp
                            (Length.meters 50)
                            (Length.meters 350)
                , camMoved = True
              }
            , Cmd.none
            )

        RequestedViewportSize ->
            let
                cmdList =
                    (Browser.Dom.getElement "controls"
                        |> Task.attempt ReturnedViewportSize
                    )
                        :: (-- camSize is only Nothing first time this runs
                            -- in this case (invoked from ChangedState)
                            -- we need to also attempt focus
                            if model.camSize == Nothing then
                                [ attemptMainFocus ]

                            else
                                []
                           )
            in
            ( model, Cmd.batch cmdList )

        ReturnedViewportSize res ->
            case res of
                Ok element ->
                    let
                        newWidth =
                            4
                                |> (/)
                                    (if element.viewport.width < 1200 then
                                        -- "small" screens use the whole width
                                        element.element.width

                                     else
                                        -- "large" screens use remaining width
                                        -- with a bit extra removed for padding
                                        element.viewport.width
                                            - element.element.width
                                            - 64
                                    )
                                |> ceiling
                                |> Pixels.int
                    in
                    ( { model
                        | camSize = Just newWidth
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        -- simulation controls
        ChangedState state ->
            case state of
                Intro ->
                    ( { model | state = state }, attemptMainFocus )

                Wrapup ->
                    let
                        togglePrecisionFirstTime =
                            if not model.showOptions then
                                -- first time we wrapup, turn on
                                True

                            else
                                -- subsequent times, just keep value
                                model.showRealPriority
                    in
                    ( { model
                        | state = state
                        , showOptions = True
                        , showRealPriority = togglePrecisionFirstTime
                      }
                    , attemptMainFocus
                    )

                Simulation dim _ _ ->
                    let
                        dimPoints =
                            case dim of
                                Dim1D ->
                                    Data.points1D

                                Dim2D ->
                                    Data.points2D

                                Dim3D ->
                                    Data.points3D

                        shouldAnimate =
                            if model.prefersReducedMotion then
                                False

                            else
                                case model.state of
                                    Intro ->
                                        False

                                    Wrapup ->
                                        False

                                    Simulation _ _ _ ->
                                        True

                        newModel =
                            { model
                                | state = state
                                , points =
                                    newCoords
                                        shouldAnimate
                                        model.points
                                        dimPoints
                            }
                    in
                    update StartedSim newModel

        StartedSim ->
            case model.state of
                Intro ->
                    ( model, Cmd.none )

                Wrapup ->
                    ( model, Cmd.none )

                Simulation dim _ _ ->
                    let
                        initialBox =
                            createBoxFromFloat 128
                                -- dimensions we are interested in need to be
                                -- offset by half unit to ensure solution is an
                                -- integer
                                (case dim of
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

                        newModel =
                            { model
                                | state = Simulation dim Start Manual
                                , simSelectedBox = Just initialBox
                                , userSelectedBox = Nothing
                                , points = initialPoints
                                , candidates =
                                    PriorityQueue.insert
                                        (toFloat <| List.length model.points)
                                        initialBox
                                        []
                                , toBeScored = []
                                , simSubBoxes = []
                            }
                    in
                    if model.camSize /= Nothing then
                        ( newModel, attemptMainFocus )

                    else
                        update RequestedViewportSize newModel

        SteppedSim ->
            simulateStep model

        SetControl c ->
            let
                newModel =
                    case model.state of
                        Intro ->
                            model

                        Wrapup ->
                            model

                        Simulation dim simState _ ->
                            { model | state = Simulation dim simState c }
            in
            if c == Skip then
                simulateStep newModel

            else
                ( newModel, Cmd.none )

        SelectedBox box ->
            ( { model | userSelectedBox = box }, Cmd.none )

        AttemptedFocus _ ->
            ( model, Cmd.none )

        -- optional toggles
        ToggledPrecision b ->
            ( { model | showRealPriority = b }, Cmd.none )

        ToggledRhombiDodecOption b ->
            ( { model | showRhombiDodec = b }, Cmd.none )

        ToggledTetraHexOption b ->
            ( { model | showTetrakis = b }, Cmd.none )

        ToggledRhombiCubeOption b ->
            ( { model | showRhombiCube = b }, Cmd.none )

        -- animation ticks
        TickedAnimation duration ->
            ( { model
                | points = List.map (animateEntity duration) model.points
              }
            , Cmd.none
            )


attemptMainFocus : Cmd Msg
attemptMainFocus =
    Browser.Dom.focus mainID
        |> Task.attempt AttemptedFocus


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
                    LineSegment3d.from entity.visualPosition
                        currentTarget.position

                direction : Maybe (Direction3d.Direction3d Coords)
                direction =
                    LineSegment3d.direction difference
            in
            if Length.inMeters (LineSegment3d.length difference) < 0.5 then
                { entity
                    | visualPosition = currentTarget.position
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
                                    entity.visualPosition
                        in
                        { entity
                            | visualPosition = newFrom
                            , futurePositions = entity.futurePositions
                        }

                    Nothing ->
                        entity

        _ ->
            entity


newCoords :
    Bool
    -> List AnimatedPoint
    -> List (Point3d.Point3d Length.Meters Coords)
    -> List AnimatedPoint
newCoords shouldAnimate entities newPoints =
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
                | position = newPoint
                , futurePositions =
                    List.map2 Target
                        (List.map interpolated [ 0.6, 0.9, 0.95, 1 ])
                        [ 4, 2, 1.5, 1 ]
            }

        setNewCoordsDirect entity newPoint =
            { entity
                | position = newPoint
                , visualPosition = newPoint
                , futurePositions = []
            }

        setFunction =
            if shouldAnimate then
                setNewCoords

            else
                setNewCoordsDirect
    in
    List.map2 setFunction entities newPoints


simulateStep : Model -> ( Model, Cmd msg )
simulateStep model =
    case model.state of
        Intro ->
            ( model, Cmd.none )

        Wrapup ->
            ( model, Cmd.none )

        Simulation dim simState control ->
            case simState of
                Solved _ _ ->
                    ( model, Cmd.none )

                _ ->
                    case model.toBeScored of
                        -- need to score all boxes before splitting any more
                        boxtoScore :: tail ->
                            scoreBox model dim control boxtoScore tail

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
                                        foundSolution model
                                            dim
                                            control
                                            poppedBox
                                            newCandidates

                                    else
                                        splitCandidate model
                                            dim
                                            control
                                            poppedBox
                                            newCandidates

                                _ ->
                                    -- Priority queue is empty
                                    -- (should have already found solution)
                                    ( model, Cmd.none )


scoreBox :
    Model
    -> Dimension
    -> SimulationControl
    -> Block3d.Block3d Length.Meters Coords
    -> List (Block3d.Block3d Length.Meters Coords)
    -> ( Model, Cmd msg )
scoreBox model dim control boxtoScore tail =
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
                | state = Simulation dim (ScoredBox score) control
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
    if control == Skip then
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
    -> Dimension
    -> SimulationControl
    -> Block3d.Block3d Length.Meters Coords
    -> PriorityQueue (Block3d.Block3d Length.Meters Coords) Float
    -> ( Model, Cmd msg )
foundSolution model dim control poppedBox newCandidates =
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
                    Simulation dim
                        (Solved
                            (Block3d.centerPoint poppedBox)
                            score
                        )
                        control
                , candidates = newCandidates
                , points = newPoints
                , simSubBoxes = []
                , simSelectedBox = Just poppedBox
            }
    in
    if control == Skip then
        simulateStep newModel

    else
        ( newModel, Cmd.none )


splitCandidate :
    Model
    -> Dimension
    -> SimulationControl
    -> Block3d.Block3d Length.Meters Coords
    -> PriorityQueue (Block3d.Block3d Length.Meters Coords) Float
    -> ( Model, Cmd msg )
splitCandidate model dim control poppedBox newCandidates =
    let
        subBoxes =
            createSubBoxes dim poppedBox

        pointsNoHighlight =
            List.map (highlight (\_ -> False))
                model.points

        widthInt =
            case Block3d.dimensions poppedBox of
                ( _, w, _ ) ->
                    w |> Length.inMeters |> round

        newModel =
            { model
                | state = Simulation dim (SplitIntoSubBoxes widthInt) control
                , candidates = newCandidates
                , toBeScored = List.reverse subBoxes
                , points = pointsNoHighlight
                , simSubBoxes = subBoxes
                , simSelectedBox = Just poppedBox
            }
    in
    if control == Skip then
        simulateStep newModel

    else
        ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        Browser.Events.onResize (\_ _ -> RequestedViewportSize)
            :: (if model.camOrbiting then
                    [ Browser.Events.onMouseMove decodeMouseMove
                    , Browser.Events.onMouseUp (Decode.succeed ReleasedMouse)
                    ]

                else
                    []
               )
            ++ (if List.any needsAnimated model.points then
                    [ Browser.Events.onAnimationFrameDelta
                        (Duration.milliseconds >> TickedAnimation)
                    ]

                else
                    []
               )
            ++ (if isFastForwardingSim model.state then
                    [ Browser.Events.onAnimationFrame (always SteppedSim) ]

                else
                    []
               )
            ++ (if
                    (model.camSize == Nothing)
                        && (model.state /= Wrapup)
                        && (model.state /= Intro)
                then
                    [ Browser.Events.onAnimationFrame <|
                        always RequestedViewportSize
                    ]

                else
                    []
               )


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MovedMouse
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


needsAnimated : AnimatedPoint -> Bool
needsAnimated entity =
    List.length entity.futurePositions > 0


{-| Can fast-forward when controls are set to fast forward and we have some
--| simulation to do that is in progress (not at initial state or solved)
-}
isFastForwardingSim : State -> Bool
isFastForwardingSim state =
    let
        isControlFastForward c =
            c == FastForward
    in
    case state of
        Intro ->
            False

        Wrapup ->
            False

        Simulation _ simState control ->
            case simState of
                Start ->
                    isControlFastForward control

                SplitIntoSubBoxes _ ->
                    isControlFastForward control

                ScoredBox _ ->
                    isControlFastForward control

                Solved _ _ ->
                    False



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header [] [ viewHeader model.state ]
        , main_ [] <|
            span [ id mainID, class "visually-hidden", tabindex -1 ]
                [ text "start of main content" ]
                :: (case model.state of
                        Intro ->
                            viewSectionIntro model.state

                        Simulation dim simState control ->
                            [ viewSectionControls model
                                model.state
                                dim
                                simState
                                control
                            , viewSectionScene model dim
                            , viewSectionCandidatesQueue
                                (model.camSize /= Nothing)
                                model.showOptions
                                model.showRealPriority
                                dim
                                model.userSelectedBox
                                model.candidates
                            , viewSectionToScoreQueue
                                (model.camSize /= Nothing)
                                dim
                                model.userSelectedBox
                                model.toBeScored
                            ]

                        Wrapup ->
                            viewSectionWrapup model.state
                   )
        , footer [] [ div [] [ text "foot" ] ]
        ]


mainID : String
mainID =
    "main-content"


viewHeader : State -> Html Msg
viewHeader state =
    let
        isCurrent compareState =
            case state of
                Intro ->
                    compareState == state

                Wrapup ->
                    compareState == state

                Simulation dim _ _ ->
                    case compareState of
                        Intro ->
                            False

                        Wrapup ->
                            False

                        Simulation sDim _ _ ->
                            sDim == dim

        buttonStep ( newState, buttonText ) =
            ( buttonText
            , button
                [ onClick <| ChangedState <| newState
                , classList [ ( "current", isCurrent newState ) ]
                , disabled <| isCurrent newState
                ]
                [ text buttonText ]
            )
    in
    div []
        [ span [] [ text "head" ]
        , Html.Keyed.node "nav" [ class "button-group" ] <|
            List.map buttonStep
                [ ( Intro, "1" )
                , ( Simulation Dim1D Start Manual, "2" )
                , ( Simulation Dim2D Start Manual, "3" )
                , ( Simulation Dim3D Start Manual, "4" )
                , ( Wrapup, "5" )
                ]
        ]


viewSectionIntro : State -> List (Html Msg)
viewSectionIntro state =
    [ section [ id "intro" ]
        [ p [] [ text "advent of code problem statement" ]
        , p [] [ text "naive approach does not work" ]
        , p [] [ text "seems imposs" ]
        , p [] [ text "programming is algorithms + data structure" ]
        , p []
            [ text "data structure explain: stack -> queue -> priority queue" ]
        , p [] [ text "simplify problem to 1 dimension to make it easier" ]
        , div [ class "align-right" ]
            [ button
                [ onClick <| ChangedState <| getNextState state
                , class "primary"
                ]
                [ text "next state" ]
            ]
        ]
    ]


viewSectionScene : Model -> Dimension -> Html Msg
viewSectionScene model dimension =
    let
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0.5 0.5 0
                , azimuth = model.camAzimuth
                , elevation = model.camElevation
                , distance = model.camDistance
                }

        axes =
            case dimension of
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
                    createEdges palette.secondary box

                Nothing ->
                    []

        boundingBoxUser =
            case model.userSelectedBox of
                Just box ->
                    createEdges palette.tertiaryAlt box

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
            List.map (createEdges palette.secondary) model.simSubBoxes
                |> List.concat
    in
    section
        [ id "scene-window" ]
    <|
        case model.camSize of
            Just cameraSize ->
                let
                    cameraWidth =
                        cameraSize
                            |> Quantity.multiplyBy 4
                            |> Quantity.clamp
                                (Pixels.int 300)
                                (Pixels.int 880)

                    cameraHeight =
                        cameraSize
                            |> Quantity.multiplyBy 3
                            |> Quantity.clamp
                                (Pixels.int 300)
                                (Pixels.int 660)
                in
                [ div
                    [ Html.Events.onMouseDown DepressedMouse
                    , Html.Events.Extra.Wheel.onWheel ChangedZoom
                    ]
                    [ Scene3d.unlit
                        { camera =
                            case model.camProjection of
                                Orthographic ->
                                    Camera3d.orthographic
                                        { viewpoint = viewpoint
                                        , viewportHeight =
                                            Quantity.half
                                                model.camDistance
                                        }

                                Perspective ->
                                    Camera3d.perspective
                                        { viewpoint = viewpoint
                                        , verticalFieldOfView = Angle.degrees 30
                                        }
                        , clipDepth = Length.meters 0.1
                        , dimensions = ( cameraWidth, cameraHeight )
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
                , div [ class "camera-controls" ] <|
                    (if model.camMoved || model.showOptions then
                        div [ class "button-group" ]
                            [ button [ onClick SelectedTopDownCamera ]
                                [ text "top down" ]
                            , button [ onClick SelectedInitialCamera ]
                                [ text "fourtyfive" ]
                            ]
                            :: (if model.showOptions then
                                    [ div []
                                        [ label [ for "toggle-projection" ]
                                            [ input
                                                [ type_ "checkbox"
                                                , onClick <| ToggledProjection
                                                , checked
                                                    (model.camProjection == Orthographic)
                                                , id "toggle-projection"
                                                ]
                                                []
                                            , text "toggle orthographic"
                                            ]
                                        , label
                                            [ for "show-rhombicube" ]
                                            [ input
                                                [ type_ "checkbox"
                                                , onClick <|
                                                    ToggledRhombiCubeOption
                                                        (not model.showRhombiCube)
                                                , checked model.showRhombiCube
                                                , id "show-rhombicube"
                                                ]
                                                []
                                            , text "draw rhombicubeoctahedron"
                                            ]
                                        ]
                                    ]

                                else
                                    []
                               )

                     else
                        []
                    )
                        ++ (if model.userSelectedBox /= Nothing then
                                [ br [] []
                                , button [ onClick <| SelectedBox Nothing ]
                                    [ text "clear selectedbox" ]
                                ]

                            else
                                []
                           )
                ]

            Nothing ->
                []


viewSectionControls :
    Model
    -> State
    -> Dimension
    -> SimulationState
    -> SimulationControl
    -> Html Msg
viewSectionControls model state dim simState control =
    let
        pluralise singular plural number =
            if number == 1 then
                singular

            else
                plural

        showCode =
            viewPseudoCode simState

        viewStatus =
            case simState of
                Solved answer score ->
                    p []
                        [ text <|
                            "Box has a width of 1 so the answer is: "
                        , viewAnswer dim answer
                        , text <|
                            " with "
                                ++ String.fromInt score
                                ++ " "
                                ++ pluralise "point" "points" score
                                ++ " in range."
                        ]

                Start ->
                    p [] [ text "Initial box added." ]

                SplitIntoSubBoxes width ->
                    p []
                        [ text <|
                            "Box has a width of "
                                ++ String.fromInt width
                                ++ ", so split into smaller boxes."
                        ]

                ScoredBox score ->
                    p []
                        [ text <|
                            "Box has "
                                ++ String.fromInt score
                                ++ " "
                                ++ pluralise "point" "points" score
                                ++ " in range.  Added to queue."
                        ]

        showButtons =
            Html.Keyed.node "div"
                [ class "status-buttons" ]
                [ case simState of
                    Solved _ _ ->
                        ( "solved"
                        , div [ class "button-group" ]
                            [ button
                                [ onClick StartedSim ]
                                [ text "reset sim" ]
                            , button
                                [ onClick <|
                                    ChangedState <|
                                        getNextState state
                                , class "primary"
                                ]
                                [ text "next dimension" ]
                            ]
                        )

                    _ ->
                        case control of
                            FastForward ->
                                ( "fastforwarding"
                                , div [ class "button-group" ]
                                    [ button
                                        [ onClick <| SetControl Manual ]
                                        [ text "stop fast forward" ]
                                    , button
                                        [ onClick <| SetControl Skip
                                        , class "primary"
                                        ]
                                        [ text "skip to end" ]
                                    ]
                                )

                            Manual ->
                                ( "stepper"
                                , div [ class "button-group" ]
                                    [ button
                                        [ onClick SteppedSim, class "primary" ]
                                        [ text "step" ]
                                    , button
                                        [ onClick <| SetControl FastForward ]
                                        [ text "fast forward" ]
                                    , button
                                        [ onClick <| SetControl Skip ]
                                        [ text "skip to end" ]
                                    ]
                                )

                            Skip ->
                                ( "skipping", div [] [ text "skipping..." ] )
                ]
    in
    section [ id "controls" ] <|
        viewSimStateExplain dim
            ++ div [ class "status-bar" ] [ showButtons, viewStatus ]
            :: showCode
            ++ (if
                    ((model.simSelectedBox /= Nothing)
                        || (model.userSelectedBox /= Nothing)
                    )
                        && model.showOptions
                then
                    [ p [] [ text "extra options" ]
                    , div [ class "toggles" ]
                        [ label
                            [ for "show-rhombidec" ]
                            [ input
                                [ type_ "checkbox"
                                , onClick <|
                                    ToggledRhombiDodecOption
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
                                    ToggledTetraHexOption
                                        (not model.showTetrakis)
                                , checked model.showTetrakis
                                , id "show-tetrakis"
                                ]
                                []
                            , text "draw tetrakis hexahahedron"
                            ]
                        ]
                    ]

                else
                    []
               )


viewSimStateExplain : Dimension -> List (Html msg)
viewSimStateExplain dimension =
    case dimension of
        Dim1D ->
            [ p [] [ text "problem is now points on a single line" ]
            , p [] [ text "now seems easier as it looks like we could do binary search" ] -- todo: is this correct term?
            , p [] [ text "box around all points, split space each time" ]
            , p [] [ text "point counts as in range if its in the box or if x value +/- range is in box" ] -- todo: explain in terms of offset to make next steps easier to explain
            , p [] [ text "to see how this works we can simulate the steps below" ]
            ]

        Dim2D ->
            [ p [] [ text "problem is now points on a plane" ]
            , p [] [ text "instead of splitting into 2, we split into 4 boxes - quadtree" ] -- todo: is this correct term?
            , p [] [ text "algorithm can stay the same just need new way to calculate in range" ]
            , p [] [ text "point counts as in range if (xoffset+yoffset <= range)" ]
            , p [] [ text "to see how this works we can simulate the steps below" ]
            ]

        Dim3D ->
            [ p [] [ text "now we are back to original problem" ]
            , p [] [ text "instead of splitting into 4, we split into 8 boxes - octree" ] -- todo: is this correct term?
            , p [] [ text "again, algorithm stays the same" ]
            , p [] [ text "point counts as in range if (xoffset+yoffset+zoffset <= range)" ]
            , p [] [ text "to see how this works we can simulate the steps below" ]
            ]


viewPseudoCode : SimulationState -> List (Html msg)
viewPseudoCode simState =
    let
        start =
            simState == Start

        solved =
            case simState of
                Solved _ _ ->
                    True

                _ ->
                    False

        split =
            case simState of
                SplitIntoSubBoxes _ ->
                    True

                _ ->
                    False

        score =
            case simState of
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
                , code
                    [ classList [ ( "active", score ), ( "skipped", split ) ] ]
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
    -> Bool
    -> Bool
    -> Dimension
    -> Maybe (Block3d.Block3d Length.Meters Coords)
    -> PriorityQueue (Block3d.Block3d Length.Meters Coords) Float
    -> Html Msg
viewSectionCandidatesQueue show showOptions showRealPriority dimension selectedBlock candidates =
    if show then
        section [ id "priority-table" ] <|
            table []
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
                :: (if showOptions then
                        [ label
                            [ for "toggle-precision" ]
                            [ input
                                [ type_ "checkbox"
                                , onClick <|
                                    ToggledPrecision
                                        (not showRealPriority)
                                , checked showRealPriority
                                , id "toggle-precision"
                                ]
                                []
                            , text "Show real priority"
                            ]
                        ]

                    else
                        []
                   )

    else
        text ""


viewSectionToScoreQueue :
    Bool
    -> Dimension
    -> Maybe (Block3d.Block3d Length.Meters Coords)
    -> List (Block3d.Block3d Length.Meters Coords)
    -> Html Msg
viewSectionToScoreQueue show dimension selectedBlock toBeScored =
    if show then
        section [ id "split-table" ]
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

    else
        text ""


viewSectionWrapup : State -> List (Html Msg)
viewSectionWrapup state =
    [ section [ id "wrapup" ]
        [ p [] [ text "congrats - now you know octrees" ]
        , p []
            [ text """can be used in more dimensions (but hard to visualise).
pattern is (2^num dimension - creating twice as many subboxes each dimension)
"""
            ]
        , p [] [ text """how are we sure found?  if there was region in range 
of more points then we would have already orocessed it into sub regions since
it would have a higher priority""" ]
        , p [] [ text "applications?" ] -- todo find good examples (boids?)
        , p []
            [ text """trick to make algo more efficient is to alter priority.
for example we were actually using (points in range - (1 + 0.001 * width))
to prioritse smaller boxes with the same number of points in range
"""
            ]
        , p [] [ text """check in range was actually checking rhombicubeoctahedron 
shape not cube in 3d""" ]
        , p [] [ text "to see priority/shape and to play with few extra toggles:" ]
        , div [ class "align-right" ]
            [ button [ onClick <| ChangedState <| getNextState state ]
                [ text "back to start"
                ]
            ]
        , div [] [ text "links - aoc, scene3d, elm" ]
        ]
    ]


getNextState : State -> State
getNextState state =
    case state of
        Intro ->
            Simulation Dim1D Start Manual

        Simulation dim _ _ ->
            case dim of
                Dim1D ->
                    Simulation Dim2D Start Manual

                Dim2D ->
                    Simulation Dim3D Start Manual

                Dim3D ->
                    Wrapup

        Wrapup ->
            Intro


createSceneEntity : AnimatedPoint -> Scene3d.Entity Coords
createSceneEntity entity =
    let
        colour =
            if entity.highlighted then
                palette.tertiary

            else
                palette.tertiaryAlt
    in
    Scene3d.point { radius = Pixels.float 2.5 }
        (Scene3d.Material.color colour)
        entity.visualPosition


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

        isDigitorDash char =
            Char.isDigit char || char == '-'

        inputID =
            (widthString ++ centreString)
                |> String.filter isDigitorDash
                |> (++) "highlight-"

        attrs =
            (if isActive then
                [ onClick <| SelectedBox Nothing ]

             else
                [ onClick <| SelectedBox (Just block) ]
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
                |> Scene3d.lineSegment (Scene3d.Material.color palette.orange)
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
                    (Scene3d.lineSegment (Scene3d.Material.color palette.pink))
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
                        (Scene3d.Material.color palette.purple)
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
