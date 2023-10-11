import deques
import hashes
import sequtils
import sets
import strutils
import tables

# part 1 - (furthestPath, _) = findFurthestRoom(map, Coord((x: 0, y: 0)))
# given a regex of instructions on how rooms are connected where NESW means
# a connection in that direction and (..|..) defines an optional path
# create a map and return the path length of the shortest path to the most
# distant room from 0,0

# part 2 - (_, thousandDoorCount) = findFurthestRoom(map, Coord((x: 0, y: 0)))
# same as above but return the number of rooms where the shortest path to the
# source passes through 1_000 doors

type
  Coord = tuple[
    x: int,
    y: int
    ]

  Room = ref object
    up: (bool, Coord)
    left: (bool, Coord)
    down: (bool, Coord)
    right: (bool, Coord)


proc `$`(coord: Coord): string =
  $coord.x & "," & $coord.y


proc readRegex(fname: string): Table[Coord, Room] =
  result = initTable[Coord, Room]()

  var cleanInput = ""

  block input:
    for line in lines(f_name):
      for character in line:
        if character == '$':
          break input
        elif character != '^':
          cleanInput.add(character)

  var
    current = Coord((x: 0, y: 0))
    next: Coord
    stack: seq[Coord]

  result[current] = Room()

  for c in cleanInput:
    case c:
      of 'N':
        next = Coord((x: current.x, y: current.y - 1))
        result[current].up = (true, next)
        if not result.hasKey(next):
          result[next] = Room()
        result[next].down = (true, current)
        current = next
      of 'E':
        next = Coord((x: current.x + 1, y: current.y))
        result[current].right = (true, next)
        if not result.hasKey(next):
          result[next] = Room()
        result[next].left = (true, current)
        current = next
      of 'S':
        next = Coord((x: current.x, y: current.y + 1))
        result[current].down = (true, next)
        if not result.hasKey(next):
          result[next] = Room()
        result[next].up = (true, current)
        current = next
      of 'W':
        next = Coord((x: current.x - 1, y: current.y))
        result[current].left = (true, next)
        if not result.hasKey(next):
          result[next] = Room()
        result[next].right = (true, current)
        current = next
      of '(':
        stack.add(current)
      of '|':
        # peek
        current = stack[^1]
      of ')':
        current = stack.pop()
      else:
        echo "badchar, ", c
        break


proc printMap(map: Table[Coord, Room]): void =
  var
    minX = int.high
    maxX = int.low
    minY = int.high
    maxY = int.low

  for key, room in map:
    maxX = max(key.x, maxX)
    maxY = max(key.y, maxY)
    minX = min(key.y, minX)
    minY = min(key.y, minY)

  # assume top row of map is all wall
  echo("")
  echo("#".repeat((maxY - minY) * 2 + 3))

  for y in minY .. maxY:
    # assume left edge of map is all wall
    var
      row1 = "#"
      row2 = "#"
    # only need to draw right and bottom sides of rooms
    # (top and left should already be done)
    for x in minX .. maxX:
      var next = Coord((x: x, y: y))
      if map.hasKey(next):
        var current = map[next]
        if x == 0 and y == 0:
          row1.add("X")
        else:
          row1.add(".")
        if current.right[0]:
          row1.add("|")
        else:
          row1.add("#")
        if current.down[0]:
          row2.add("-")
        else:
          row2.add("#")
        row2.add("#")
      # is there a void in the map?
      else:
        row1.add("##")
        row2.add("##")
    echo(row1)
    echo(row2)


proc findFurthestRoom(map: Table[Coord, Room], source: Coord): (seq[Coord], int) =
  var
    q = initDeque[seq[Coord]]()
    visited = initHashSet[Coord](1024)
    thousandDoorCount = 0

  # start breadth first search from source
  q.addLast @[source]

  # while still have cells to search
  while q.len > 0:
    var
      currentPath = q.popFirst()
      pathEnd = currentPath[^1]

    if currentPath.len > 1_000:
      thousandDoorCount.inc()

    # add neighbouring cells to bfs queue
    var
      neighbours: seq[Coord]
      room = map[pathEnd]
    if room.up[0]:
      neighbours.add(Coord((x: pathEnd.x, y: pathEnd.y - 1)))
    if room.right[0]:
      neighbours.add(Coord((x: pathEnd.x + 1, y: pathEnd.y)))
    if room.down[0]:
      neighbours.add(Coord((x: pathEnd.x, y: pathEnd.y + 1)))
    if room.left[0]:
      neighbours.add(Coord((x: pathEnd.x - 1, y: pathEnd.y)))

    for neighbour in neighbours:
      # only interested in newly found neighbours
      if not visited.contains(neighbour) and not q.anyIt(it[it.high] == neighbour):
        var nextPath = currentPath
        nextPath.add(neighbour)
        q.addLast(nextPath)

    # remember this cell has been visited already
    visited.incl(pathEnd)

    # there are no more rooms to visit so this is the longest shortest path
    # to a room
    if q.len == 0:
      return (currentPath, thousandDoorCount)


let
  filename = "input.txt"
  map = readRegex(filename)
  debug = false

if debug:
  printMap(map)

let (furthestPath, thousandDoorCount) = findFurthestRoom(map, Coord((x: 0, y: 0)))

echo "Part 1: ", furthestPath.len - 1

echo "Part 2: ", thousandDoorCount

