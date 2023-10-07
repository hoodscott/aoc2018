import strutils, heapqueue

# part 1 - botsInRange(..).len
# given a list of nanobots with position in 3d space and a signal range find the
# bot with the highest strength (range) and count how many bots are in range of
# it

# part 2 - distance(strongestSignal, Vec3(x: 0, y: 0, z: 0))
# given the above list of bots, find the point in space where you are in range
# of the most bots and return manhattan distance from this point to 0,0,0
# (sum of x,y,z)


type
  Vec3 = object
    x, y, z: int

  Nanobot = object
    pos: Vec3
    strength: int

  Nanobox = object
    pos: Vec3
    size: int
    botCount: int


proc `<`(a, b: Nanobox): bool =
  if a.botCount == b.botCount:
    return (a.pos.x + a.pos.y + a.pos.z) < (b.pos.x + b.pos.y + b.pos.z)
  else:
    # we want lowest first so flip sign here
    return a.botCount > b.botCount


proc readInput(fName: string): seq[Nanobot] =
  for line in lines fName:
    var x = line.split({'<', '>', ' ', '=', ','})
    result.add(Nanobot(pos: Vec3(x: parseInt(x[2]), y: parseInt(x[3]),
        z: parseInt(x[4])), strength: parseInt(x[^1])))


proc findMostPowerful(bots: seq[Nanobot]): Nanobot =
  var max = int.low
  for bot in bots:
    if bot.strength > max:
      max = bot.strength
      result = bot


proc distance(a, b: Vec3): int =
  abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)


proc botsInRange(bots: seq[Nanobot], source: Vec3, strength: int): seq[Nanobot] =
  for bot in bots:
    if distance(bot.pos, source) <= strength:
      result.add(bot)

# find smallest cube with a length of a power of 2 that contains all points
proc calcInitialBounds(bots: seq[Nanobot]): (Vec3, int) =
  var
    minX, minY, minZ = int.high
    maxX, maxY, maxZ = int.low

  for bot in bots:
    minX = min(minX, bot.pos.x)
    minY = min(minY, bot.pos.y)
    minZ = min(minZ, bot.pos.z)
    maxX = max(maxX, bot.pos.x)
    maxY = max(maxY, bot.pos.y)
    maxZ = max(maxZ, bot.pos.z)

  let minV = Vec3(x: minX, y: minY, z: minZ)

  var size = 2
  while minX + size < maxX or minY + size < maxY or minZ + size < maxZ:
    size *= 2

  return (minV, size)


proc countBotsInRange(bots: seq[Nanobot], topLeftPos: Vec3, size: int): int =
  for bot in bots:
    var distance = 0

    # how far out of range in the x
    if bot.pos.x < topLeftPos.x:
      distance += topLeftPos.x - bot.pos.x
    elif bot.pos.x > topLeftPos.x + size - 1:
      distance += bot.pos.x - (topLeftPos.x + size - 1)

    # how far out of range in the y
    if bot.pos.y < topLeftPos.y:
      distance += topLeftPos.y - bot.pos.y
    elif bot.pos.y > topLeftPos.y + size - 1:
      distance += bot.pos.y - (topLeftPos.y + size - 1)

    # how far out of range in the z
    if bot.pos.z < topLeftPos.z:
      distance += topLeftPos.z - bot.pos.z
    elif bot.pos.z > topLeftPos.z + size - 1:
      distance += bot.pos.z - (topLeftPos.z + size - 1)

    # bots signal strength can bring it back into range
    if distance <= bot.strength:
      result.inc()


proc createSubBox(bots: seq[Nanobot], startPos: Vec3, offset: Vec3,
    size: int): Nanobox =
  let
    newPos = Vec3(x: startPos.x + offset.x, y: startPos.y + offset.y,
        z: startPos.z + offset.z)
    botCount = countBotsInRange(bots, newPos, size)

  return Nanobox(pos: newPos, size: size, botCount: botCount)


proc findStrongestSignal(bots: seq[Nanobot]): Vec3 =
  let (startPos, size) = calcInitialBounds(bots)

  # queue sorted by smallest botCount (if tied then closest pos to 0,0,0)
  var queue = toHeapQueue([Nanobox(pos: startPos, size: size,
      botCount: bots.len)])

  while queue.len > 0:
    let box = queue.pop()

    # when octree reduced to single point, that's our best signal strength
    if box.size == 1:
      return box.pos

    # add 8 sub cubes to queue (octree)
    let halfSize = box.size div 2
    queue.push(createSubBox(bots, box.pos,
      Vec3(x: 0, y: 0, z: 0), halfSize))
    queue.push(createSubBox(bots, box.pos,
      Vec3(x: halfSize, y: 0, z: 0), halfSize))
    queue.push(createSubBox(bots, box.pos,
      Vec3(x: 0, y: halfSize, z: 0), halfSize))
    queue.push(createSubBox(bots, box.pos,
      Vec3(x: halfSize, y: halfSize, z: 0), halfSize))
    queue.push(createSubBox(bots, box.pos,
      Vec3(x: 0, y: 0, z: halfSize), halfSize))
    queue.push(createSubBox(bots, box.pos,
      Vec3(x: halfSize, y: 0, z: halfSize), halfSize))
    queue.push(createSubBox(bots, box.pos,
      Vec3(x: 0, y: halfSize, z: halfSize), halfSize))
    queue.push(createSubBox(bots, box.pos,
      Vec3(x: halfSize, y: halfSize, z: halfSize), halfSize))

  echo "no signal found"

let
  filename = "input.txt"
  nanobots = readInput(filename)
  mostpowerful = findMostPowerful(nanobots)
  strongestSignal = findStrongestSignal(nanobots)

echo "Part 1: ", botsInRange(nanobots, mostpowerful.pos,
    mostpowerful.strength).len

echo "Part 2: ", distance(strongestSignal, Vec3(x: 0, y: 0, z: 0))
