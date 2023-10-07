import strutils, tables, heapqueue, algorithm

# part 1 - calcRisk(map, target)
# given an input depth and target coords, create a map of the cave where
# each region has a certain erosion level that determines what type (Rocky,
# Narrow, or Wet).
# calculate the risk of the regions in the smallest rectangle containing 0,0
# and the target where Rocky counts as 0, Wet counts as 1, and Narrow counts
# as 2

# part 2 - example.nim
# try to find shortest path from 0,0 to target.  twist is different regions
# need different equipment and switching equipment costs 7 minutes (moving
# always takes 1 minute)
# couldnt get my solution working here - my shortest path was 30 too big and
# it kept iterating to the right instead of down

type
  Coord = tuple[x: int, y: int]

  Rtype = enum
    Rocky, Wet, Narrow

  Tool = enum
    Neither, Gear, Torch

  Region = ref object
    rtype: Rtype
    eroLevel: int

  # pathfinding stuff
  CoordP = object
    coord: Coord
    equip: Tool
    cameFrom: (bool, (Coord, Tool))
    gScore = int.high
    fScore = int.high


proc readInput(fName: string): (int, Coord) =
  var
    firstLine = true
    depth: int
    target: Coord

  for line in lines fName:
    if firstLine and line.split().len == 2:
      depth = parseInt(line.split()[1])
      firstLine = false
    else:
      var temp = line.split()[1].split(",")
      target = Coord((x: parseInt(temp[0]), y: parseInt(temp[1])))

  return (depth, target)


proc createMap(depth: int, target: Coord): Table[Coord, Region] =
  result = initTable[Coord, Region]()

  let
    bufferX = target.x * 4
    bufferY = target.y * 2

  for x in 0 .. (target.x + bufferX):
    for y in 0 .. (target.y + bufferY):

      var geoIndex: int
      if (x == 0 and y == 0) or (x == target.x and y == target.y):
        geoIndex = 0
      elif y == 0:
        geoIndex = x * 16807
      elif x == 0:
        geoIndex = y * 48271
      else:
        geoIndex = result[Coord((x: x - 1, y: y))].eroLevel * result[Coord((
            x: x, y: y - 1))].eroLevel

      let
        xy = Coord((x: x, y: y))
        region = Region()

      region.eroLevel = (geoIndex + depth) mod 20183
      region.rtype = case (region.eroLevel mod 3):
        of 0: Rocky
        of 1: Wet
        of 2: Narrow
        else: Narrow
      result[xy] = region


proc printMap(map: Table[Coord, Region], target: Coord): void =
  for y in 0 .. target.y:
    var line = ""
    for x in 0 .. target.x:
      line.add(case map[Coord((x: x, y: y))].rtype:
      of Rocky: "."
      of Wet: "="
      of Narrow: "|")
    echo(line)


proc calcRisk(map: Table[Coord, Region], target: Coord): int =
  for y in 0 .. target.y:
    for x in 0 .. target.x:
      result.inc(case map[Coord((x: x, y: y))].rtype:
      of Rocky: 0
      of Wet: 1
      of Narrow: 2)


proc reconstructPath(map: Table[(Coord, Tool), CoordP], endPath: CoordP): seq[(
    Coord, Tool)] =
  var
    current = endPath

  result.add((current.coord, current.equip))
  while current.cameFrom[0]:
    current = map[current.cameFrom[1]]
    result.add((current.coord, current.equip))

proc manhattanDistance(a: Coord, b: Coord, tool: Tool): int =
  result = (abs(a.x-b.x) + abs(a.y-b.y))
  if tool != Torch:
    result.inc(7)

proc calcDistance(tool: Tool, a: Region, b: Region): (int, Tool) =
  if a.rtype == b.rtype:
    return (1, tool)

  case a.rtype:
    of Rocky:
      case b.rtype:
        of Rocky:
          return (1, tool)

        of Wet:
          case tool:
            of Torch:
              return (8, Gear)

            of Gear, Neither:
              return (1, tool)

        of Narrow:
          case tool:
            of Gear:
              return (8, Torch)

            of Torch, Neither:
              return (1, tool)

    of Wet:
      case b.rtype:
        of Wet:
          return (1, tool)

        of Rocky:
          case tool:
            of Neither:
              return (8, Gear)

            of Gear, Torch:
              return (1, tool)

        of Narrow:
          case tool:
            of Gear:
              return (8, Neither)

            of Neither, Torch:
              return (1, tool)

    of Narrow:
      case b.rtype:
        of Narrow:
          return (1, tool)

        of Rocky:
          case tool:
            of Neither:
              return (8, Torch)

            of Torch, Gear:
              return (1, tool)

        of Wet:
          case tool:
            of Torch:
              return (8, Neither)

            of Neither, Gear:
              return (1, tool)


proc heapQueueContains(q: HeapQueue[CoordP], val: Coord): bool =
  for i in 0..<q.len:
    if q[i].coord == val:
      return true
  return false


proc `<`(a, b: CoordP): bool = a.fScore < b.fScore

# A* finds a path from start to goal.
# h is the heuristic function. h(n) estimates the cost to reach goal from node n.
proc pathFindAStar(map: Table[Coord, Region], start: Coord, goal: Coord): seq[(
    Coord, Tool)] =
  var checked = 0
  # For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from the start
  # to n currently known.
  # cameFrom := an empty map

  var mapScore = initTable[(Coord, Tool), CoordP]()

  # For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
  # mapScore[start].gScore = 0

  # For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
  # how cheap a path could be from start to finish if it goes through n.
  # fScore := map with default value of Infinity
  # map[start].fScore = manhattanDistance(start, goal, Torch)

  # map[start].equip = Torch

  var cp = CoordP(coord: start, equip: Torch, gScore: 0,
      fScore: manhattanDistance(start, goal, Torch))
  mapScore[(start, Torch)] = cp


  # The set of discovered nodes that may need to be (re-)expanded.
  # Initially, only the start node is known.
  # This is usually implemented as a min-heap or priority queue rather than a hash-set.
  var openSet = toHeapQueue([cp])

  while openSet.len > 0:
    checked.inc()
    # This operation can occur in O(Log(N)) time if openSet is a min-heap or a priority queue
    # var current = the node in openSet having the lowest fScore[] value
    var current = openSet.pop()
    # echo "check ", current
    # var x = stdin.readLine()
    # for i in 0..<openSet.len:
    #   # echo openSet[i]
    if current.coord == goal:
      if current.equip == Torch:
        echo "checked ", checked
        return reconstructPath(mapScore, current)
      else:
        # weird case when we find the end but need to change tools
        var
          neighbour = current.coord
          tool = Torch
          gScoreMaybe = current.gScore + 7

        mapScore[(current.coord, Torch)] = CoordP(cameFrom: (true, (
            current.coord, current.equip)), gScore: gScoreMaybe,
                fScore: gScoreMaybe + manhattanDistance(
            neighbour, goal, tool), equip: tool, coord: current.coord)

        if not heapQueueContains(openSet, neighbour):
          openSet.push(mapScore[(neighbour, tool)])
        else:
          # may need to update priority
          for i in 0..<openSet.len:
            if openSet[i].coord == neighbour and openSet[i].equip == tool:
              if openSet[i].fScore > mapScore[(neighbour, tool)].fScore:
                # remove and re-add if priority has changed
                openSet.del(i)
                openSet.push(mapScore[(neighbour, tool)])
              break

        continue

    # openSet.Remove(current)

    var neighbours: seq[Coord]
    if current.coord.y > 0:
      neighbours.add(Coord((x: current.coord.x, y: current.coord.y - 1))) # up
    neighbours.add(Coord((x: current.coord.x + 1, y: current.coord.y))) # right
    neighbours.add(Coord((x: current.coord.x, y: current.coord.y + 1))) # down
    if current.coord.x > 0:
      neighbours.add(Coord((x: current.coord.x - 1, y: current.coord.y))) # left

    for neighbour in neighbours:
      # d(current,neighbor) is the weight of the edge from current to neighbor
      # gScoreMaybe is the distance from start to the neighbor through current
      var
        (distance, tool) = calcDistance(current.equip, map[current.coord], map[neighbour])
        gScoreMaybe = current.gScore + distance

      if not mapScore.hasKey((neighbour, tool)):
        mapScore[(neighbour, tool)] = CoordP(coord: neighbour, equip: tool)

      if gScoreMaybe < mapScore[(neighbour, tool)].gScore:
        # This path to neighbor is better than any previous one. Record it!
        mapScore[(neighbour, tool)].cameFrom = (true, (current.coord,
            current.equip))
        mapScore[(neighbour, tool)].gScore = gScoreMaybe
        mapScore[(neighbour, tool)].fScore = gScoreMaybe + manhattanDistance(
            neighbour, goal, tool)
        mapScore[(neighbour, tool)].equip = tool
        if not heapQueueContains(openSet, neighbour):
          openSet.push(mapScore[(neighbour, tool)])
        # else:
          # # may need to update priority
          # for i in 0..<openSet.len:
          #   if openSet[i].coord == neighbour and openSet[i].equip == tool:
          #     if openSet[i].fScore > mapScore[(neighbour, tool)].fScore:
          #       # remove and re-add if priority has changed
          #       openSet.del(i)
          #       openSet.push(mapScore[(neighbour, tool)])
          #     break

        # Open set is empty but goal was never reached so return empty path
  echo "goal never reached?"
  return @[]


let
  input = "input.txt"
  (depth, target) = readInput(input)
  map = createMap(depth, target)
  debug = true

if debug:
  printMap(map, target)

echo "Part 1: ", calcRisk(map, target)

var
  toolSwitches = 0
  prevTool = Torch
let path = pathFindAStar(map, Coord((x: 0, y: 0)), target)

echo ""
var time = path.len - 1 # path also contains 0,0
echo "len-1 ", time

if path[0][0] == path[1][0]:
  time.dec()
  echo "last turn switching tools so dec len ", time

for i in path.reversed():
  # echo i, ", ", map[i[0]].rtype
  if i[1] != prevTool:
    toolSwitches.inc()
  prevTool = i[1]
if prevTool != Torch:
  toolSwitches.inc()

time.inc(toolSwitches * 7)
echo "plus ", toolSwitches, " switches = ", time

#echo path.len - 2, " + ", toolSwitches, " = ", path.len - 1 + (toolSwitches * 7)
#1000 + 5 = 1036
