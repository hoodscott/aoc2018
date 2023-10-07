import strutils

# part 1 - botsInRange(..).len
# given a list of nanobots with position in 3d space and a signal range find the
# bot with the highest strength (range) and count how many bots are in range of
# it

# part 2 -
#


type
  Vec3 = object
    x, y, z: int

  Nanobot = object
    pos: Vec3
    strength: int


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


let
  filename = "input.txt"
  nanobots = readInput(filename)
  mostpowerful = findMostPowerful(nanobots)

echo "Part 1: ", botsInRange(nanobots, mostpowerful.pos,
    mostpowerful.strength).len
