import strutils, sequtils, algorithm

# part 1 - flowFrom(earth, getSource(earth))
# given an input of coordinates indicating where solid clay is and assuming
# there is a source of water at position (0,500), simulate water falling
# downwards and pooling on clay and return the number of tiles the water can
# reach.  to prevent infinite counting, only consider tiles with y values that
# are within the same range as the input values

# part 2 - (..., countStillWater) = countWet(earth, minY)
# same as above but only count the pooled up water (WaterStill)

let debug = false

type
  Cell = ref object of RootObj
    material: Material

  Material = enum
    Sand, WaterStill, Clay, Source, WaterRunning

  Earth = seq[seq[Cell]]

  Coord = tuple[x: int, y: int]


proc buildEarth(f_name: string): (Earth, int) =
  var
    coords: seq[Coord]
    maxY = int.low
    minY = int.high
    maxX = 500
    minX = 500
    extraX = 16
    extraXHalf = extraX div 2

  # turn input file into sequence of coordinates
  for line in lines f_name:
    var xy = line.split(", ")
    xy.sort()

    let
      xs = xy[0][2 .. ^1].split("..")
      ys = xy[1][2 .. ^1].split("..")

    if xs.len > 1:
      let y = parseInt(ys[0])
      for x in parseInt(xs[0]) .. parseInt(xs[1]):
        coords.add((x, y))

        if x > maxX:
          maxX = x
        if x < minX:
          minX = x
        if y > maxY:
          maxY = y
        if y < minY:
          minY = y
    elif ys.len > 1:
      let x = parseInt(xs[0])
      for y in parseInt(ys[0]) .. parseInt(ys[1]):
        coords.add((x, y))

        if x > maxX:
          maxX = x
        if x < minX:
          minX = x
        if y > maxY:
          maxY = y
        if y < minY:
          minY = y
    else:
      let
        x = parseInt(xs[0])
        y = parseInt(ys[0])
      coords.add((x, y))

      if x > maxX:
        maxX = x
      if x < minX:
        minX = x
      if y > maxY:
        maxY = y
      if y < minY:
        minY = y

  var e = newSeqWith(maxY + 1, newSeqWith(maxX - minX + extraX, Cell(
      material: Sand)))

  # water spring at 500,0
  e[0][500 - minX + extraXHalf].material = Source

  # coords correspond to clay in the earth
  for coord in coords:
    e[coord.y][coord.x - minX + extraXHalf].material = Clay

  return (e, minY)


proc showEarth(e: Earth): void =
  for line in e:
    var lineStr = ""
    for cell in line:
      case cell.material:
        of Sand:
          lineStr.add(".")
        of Clay:
          lineStr.add("#")
        of WaterStill:
          lineStr.add("~")
        of Source:
          lineStr.add("+")
        of WaterRunning:
          lineStr.add("|")
    echo lineStr
  echo ""


proc countWet(e: Earth, minY: int): (int, int) =
  var total = 0
  var totalStill = 0

  for y, line in e:
    if y >= minY:
      for cell in line:
        case cell.material:
          of WaterRunning, WaterStill:
            total.inc()
            if cell.material == WaterStill:
              totalStill.inc()
          else:
            discard

  return (total, totalStill)


proc getSource(e: Earth): Coord =
  for y, line in e:
    for x, cell in line:
      if cell.material == Source:
        return Coord((x, y))


var flowFrom: proc(e: var Earth, source: Coord): void


proc spreadFrom(e: var Earth, source: Coord): void =
  if debug:
    echo "spread", source
    showEarth(e)

  var
    sameLevel = @[e[source.y][source.x]]
    dX: int

  # spread to the left
  dX = 1
  var
    left = e[source.y][source.x - dX]
    hasLeftLeak = false

  while (left.material == Sand or left.material == WaterRunning) and
      not hasLeftLeak:
    sameLevel.add(left)
    hasLeftLeak = e[source.y + 1][source.x - dX].material == Sand
    if hasLeftLeak:
      flowFrom(e, Coord((source.x - dX, source.y)))

    dX.inc()
    left = e[source.y][source.x - dX]

  # spread to the right
  dX = 1
  var
    right = e[source.y][source.x + dX]
    hasRightLeak = false

  while (right.material == Sand or right.material == WaterRunning) and
      not hasRightLeak:
    sameLevel.add(right)
    hasRightLeak = e[source.y + 1][source.x + dX].material == Sand
    if hasRightLeak:
      flowFrom(e, Coord((source.x + dX, source.y)))

    dX.inc()
    right = e[source.y][source.x + dX]


  # is this water level running or still?
  for c in sameLevel:
    if hasLeftLeak or hasRightLeak:
      # watch out for special case where water already settled in previous recurse
      if c.material != WaterStill:
        c.material = WaterRunning
    else:
      c.material = WaterStill

  # if this water has settled (has no leaks) then spread water on level above
  if not (hasLeftLeak or hasRightLeak):
    spreadFrom(e, Coord((source.x, source.y - 1)))



flowFrom = proc(e: var Earth, source: Coord): void =
  if debug:
    echo "flow", source
    showEarth e

  var
    depth = 1
    below = e[source.y + depth][source.x]

  # mark the source as running water unless it is the literal source block
  if e[source.y][source.x].material != Source:
    e[source.y][source.x].material = WaterRunning

  # travel downwards while possible
  while below.material == Sand or below.material == WaterRunning:
    # water already pooling here so don't need to process this flow any more
    if below.material == WaterRunning:
      return

    below.material = WaterRunning
    depth.inc()

    # check we are still in bounds for next block below
    if source.y + depth < e.len:
      below = e[source.y + depth][source.x]
    else:
      # if not in bounds, water drains into the void
      return

  # flowing onto an obstacle means spreading water at the level above
  spreadFrom(e, Coord((source.x, source.y + depth - 1)))


let
  fileName = "input.txt"
  displayFinal = true

var (earth, minY) = buildEarth(fileName)

if debug:
  showEarth earth

flowFrom(earth, getSource(earth))

if debug or displayFinal:
  showEarth earth

var (countTotalWater, countStillWater) = countWet(earth, minY)

echo "Part 1: ", countTotalWater
echo "Part 2: ", countStillWater
