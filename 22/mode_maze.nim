import strutils, tables

# part 1 - calcRisk(map, target)
# given an input depth and target coords, create a map of the cave where
# each region has a certain erosion level that determines what type (Rocky,
# Narrow, or Wet).
# calculate the risk of the regions in the smallest rectangle containing 0,0
# and the target where Rocky counts as 0, Wet counts as 1, and Narrow counts
# as 2

# part 2 -
#

type
  Coord = tuple[x: int, y: int]

  Rtype = enum
    Rocky, Wet, Narrow

  Region = ref object
    rtype: Rtype
    eroLevel: int


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

  for x in 0 .. target.x:
    for y in 0 .. target.y:

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


let
  input = "input.txt"
  (depth, target) = readInput(input)
  map = createMap(depth, target)
  debug = false

if debug:
  printMap(map, target)

echo "Part 1: ", calcRisk(map, target)
