import strutils, sequtils, tables

# part 1 - createConstellations(stars).len
# given a list of 4d coordinates, group the coordinates into constellations.
# if any two stars are a distance of <4 away then they are in the same
# constellation.  return the number of constellations

# part 2 -
#

type
  Star = tuple[x, y, z, t: int]


proc manhattanDistance(a, b: Star): int =
  abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.t - b.t)


proc readInput(fName: string, boost = 0): seq[Star] =
  for line in lines fName:
    var lineInts = line.split(",").map(parseInt)
    result.add(Star((x: lineInts[0], y: lineInts[1], z: lineInts[2],
        t: lineInts[3])))


proc createConstellations(stars: seq[Star]): Table[int, seq[Star]] =
  result = initTable[int, seq[Star]]()
  var id = 0

  for star in stars:
    var deletedIds: seq[int]
    var insert = @[star]

    for constellationId, constellation in pairs(result):
      var canFit = false
      for otherStar in constellation:
        if manhattanDistance(star, otherStar) < 4:
          canFit = true
          break
      if canFit:
        insert.add(constellation)
        deletedIds.add(constellationId)

    for delId in deletedIds:
      result.del(delId)

    result[id] = insert
    id.inc()



const filename = "input.txt"
let stars = readInput(filename)

let constellations = createConstellations(stars)

echo "Part 1: ", constellations.len
