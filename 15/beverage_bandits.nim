import sequtils
import deques
import algorithm
import sets
import tables
import hashes


# part 1 - while true
# given a map of initial state of combat - simulate rounds until one unit cannot
# find a target and return the round number * sum of hp of all remaining units

# part 2 -

type
  Pos = ref object
    x: int
    y: int

  UnitType = enum Elf, Goblin

  Unit = ref object
    faction: UnitType
    pos: Pos
    hp: int
    attack: int
    isDead: bool

  CellContent = enum Wall, Space

  Cell = ref object
    content: CellContent


proc `==` (a: Pos, b: Pos): bool =
  a.x == b.x and a.y == b.y


proc `!=` (a: Pos, b: Pos): bool =
  not (a == b)


proc hash(p: Pos): Hash =
  result = p.x.hash !& p.y.hash
  result = !$result


proc read_file(f_name: string): (seq[seq[Cell]], seq[Unit]) =
  var
    y = 0
    bf: seq[seq[Cell]]
    units: seq[Unit]
  for line in lines f_name:
    var cellRow: seq[Cell]
    for x, symbol in line:
      var
        cell = Cell(content: if symbol == '#': Wall else: Space)
      if symbol == 'E' or symbol == 'G':
        units.add(Unit(faction: if symbol == 'E': Elf else: Goblin, pos: Pos(
            x: x, y: y), hp: 200, attack: 3, isDead: false))
      cellRow.add(cell)
    bf.add(cellRow)
    y.inc()
  return (bf, units)


proc `$`(cell: Cell): string =
  case cell.content:
    of Wall:
      return "#"
    of Space:
      return "."


proc `$`(unit: Unit): string =
  case unit.faction:
    of Elf:
      return "e"
    of Goblin:
      return "g"


proc `$`(pos: Pos): string =
  result = $pos.x & "," & $pos.y



proc print_battlefield(b: seq[seq[Cell]], units: seq[Unit]): void =
  for y, line in b:
    var printLine: string
    for x, cell in line:
      var printedUnit = false
      for unit in units:
        if unit.pos.x == x and unit.pos.y == y:
          printLine.add($unit)
          printedUnit = true
          break
      if not printedUnit:
        printLine.add($cell)
    echo printLine
  echo ""


proc cmpUnits(a: Unit, b: Unit): int =
  result = cmp(a.pos.y, b.pos.y)
  if result == 0:
    result = cmp(a.pos.x, b.pos.x)


proc cmpTargetHp(a: Unit, b: Unit): int =
  result = cmp(a.hp, b.hp)
  if result == 0:
    result = cmpUnits(a, b)


proc targetInRange(pos: Pos, targets: seq[Unit]): seq[Unit] =
  for target in targets:
    if pos.x == target.pos.x and abs(pos.y-target.pos.y) == 1:
      result.add(target)
    elif pos.y == target.pos.y and abs(pos.x - target.pos.x) == 1:
      result.add(target)


proc getOpenNeighbours(pos: Pos, bf: seq[seq[Cell]], units: seq[Unit]): seq[Pos] =
  if bf[pos.y - 1][pos.x].content != Wall:
    var unitsAtCell = units.filterIt(it.pos.x == pos.x and it.pos.y == pos.y -
        1 and not it.isDead)
    if unitsAtCell.len == 0:
      result.add(Pos(x: pos.x, y: pos.y - 1))
  if bf[pos.y][pos.x - 1].content != Wall:
    var unitsAtCell = units.filterIt(it.pos.x == pos.x - 1 and it.pos.y ==
        pos.y and not it.isDead)
    if unitsAtCell.len == 0:
      result.add(Pos(x: pos.x - 1, y: pos.y))
  if bf[pos.y][pos.x + 1].content != Wall:
    var unitsAtCell = units.filterIt(it.pos.x == pos.x + 1 and it.pos.y ==
        pos.y and not it.isDead)
    if unitsAtCell.len == 0:
      result.add(Pos(x: pos.x + 1, y: pos.y))
  if bf[pos.y + 1][pos.x].content != Wall:
    var unitsAtCell = units.filterIt(it.pos.x == pos.x and it.pos.y == pos.y +
        1 and not it.isDead)
    if unitsAtCell.len == 0:
      result.add(Pos(x: pos.x, y: pos.y + 1))


proc endPath (path: seq[Pos]): Pos =
  path[path.high]


proc breadthFirstSearch(bf: seq[seq[Cell]], units: seq[Unit], source: Pos,
    targets: HashSet[Pos]): TableRef[Pos, seq[Pos]] =
  var
    q = initDeque[seq[Pos]]()
    visited = initHashSet[Pos](1024)

  result = newTable[Pos, seq[Pos]]()

  # start bfs from source
  q.addLast @[source]

  # while still have cells to search and not found path to all targets
  while q.len > 0 and result.len < targets.len:
    var
      currentPath = q.popFirst()
      pathEnd = endPath(currentPath)

    # we found the path to a target
    if targets.contains(pathEnd):
      result[pathEnd] = currentPath

    # add neighbouring cells to bfs queue
    for pos in getOpenNeighbours(pathEnd, bf, units):
      if not visited.contains(pos) and not q.anyIt(it[it.high] == pos):
        var nextPath = currentPath
        nextPath.add(pos)
        q.addLast(nextPath)

    # remember this cell has been visited already
    visited.incl(pathEnd)


proc cmpPos(a: Pos, b: Pos): int =
  result = cmp(a.y, b.y)
  if result == 0:
    result = cmp(a.x, b.x)


func cmpDestination(a: seq[Pos], b: seq[Pos]): int =
  return cmpPos(endPath(a), endPath(b))


proc selectDestination(paths: seq[seq[Pos]]): Pos =
  # if there is only one path - just return the last position from it
  if paths.len == 1:
    return endPath(paths[0])

  # sort paths of the shortest length by reading order of their destinations
  var
    shortestLen = min(paths.mapIt(it.len))
    shortPaths = paths.filterIt(it.len == shortestLen)
  shortPaths.sort(cmpDestination)
  # shortPaths = shortPaths.filterIt(endPath(it) == endPath(shortPaths[0])) should be able to just take first, no?

  # assert shortPaths.len > 0
  # if shortPaths.len == 1:
  return endPath(shortPaths[0])


proc selectMove(bf: seq[seq[Cell]], units: seq[Unit], unit: Unit,
    chosenDest: Pos): Pos =
  var
    validMoves = getOpenNeighbours(unit.pos, bf, units)
    otherUnits = units.filterIt(it.pos != unit.pos)
    shortestPathLength = int.high
    bestMoves: seq[Pos]

  # simulate making any of the 4 possible moves
  for move in validMoves:
    # find paths to destination after making a move
    var destinations = initHashSet[Pos]()
    destinations.incl chosenDest
    var foundPaths = breadthFirstSearch(bf, otherUnits, move, destinations)
    if foundPaths.len == 0: continue #this should never happen
    
    # keep track of which move moves us closer to target the quickest
    var pathLen = foundPaths[chosenDest].len
    if pathLen < shortestPathLength:
      shortestPathLength = pathLen
      bestMoves = @[move]
    elif pathLen == shortestPathLength:
      bestMoves.add move

  # take the move that moves us closest breaking ties by reading order
  return bestMoves.sorted(cmpPos)[0]


proc simulate_round(bf: seq[seq[Cell]], units: var seq[Unit]): (
    bool, seq[seq[Cell]], seq[Unit]) =
  # for each unit in reading order (by row)
  units.sort(cmpUnits)
  for unit in units:
    if unit.isDead: continue

    var targets: seq[Unit]

    # identify targets
    for other in units:
      if not other.isDead and other.faction != unit.faction:
        targets.add(other)

    # if no targets found then game is over
    if targets.len == 0:
      return (true, bf, units)
    else:
      var closeTargets = targetInRange(unit.pos, targets)

      # are any targets in range
      if closeTargets.len == 0:
        # if not can we try to move closer
        var
          hasMoved = false
          goals = initHashSet[Pos]()
          newPos: Pos

        # identify target's open adjacent squares (not diag)
        for target in targets:
          for pos in getOpenNeighbours(target.pos, bf, units):
            goals.incl(pos)

        # if there is a space, then try to path to it
        if goals.len > 0:
          # find rangest that is closest by number of steps
          var paths = toSeq(breadthFirstSearch(bf, units, unit.pos, goals).values)

          # if no goals spaces are reachable then we don't move
          if paths.len > 0:
            # break ties by reading order
            var chosenDest = selectDestination(paths)
            # take 1 step towards target by shortest path route
            # (if multiple shortest paths, break ties by reading order)
            newPos = selectMove(bf, units, unit, chosenDest)
            unit.pos = newPos
            hasMoved = true

        # update closeTargets if we have moved
        if hasMoved:
          closeTargets = targetInRange(newPos, targets)

      # if now or already were in range then attack
      if closeTargets.len != 0:
        # choose target with fewest hp (break ties on reading order)
        closeTargets.sort(cmpTargetHp)
        # deal <attack_power> damage to target
        closeTargets[0].hp.dec(unit.attack)
        # if target hp <=0 target dies and is removed from map
        if closeTargets[0].hp <= 0:
          closeTargets[0].isDead = true


  (false, bf, units)

proc sum_hp(units: seq[Unit]): int =
  for unit in units:
    result.inc(unit.hp)

var
  input_file = "input.txt"
  battlefield: seq[seq[Cell]]
  units: seq[Unit]
  debug = false
  roundNum = 0
  gameOver: bool

(battlefield, units) = read_file(input_file)

while true:
  if roundNum == 505:
    break

  if debug:
    print_battlefield(battlefield, units)

  (gameOver, battlefield, units) = simulate_round(battlefield, units)
  units = units.filterIt(not it.isDead)

  if gameOver:
    break

  roundNum.inc(1)

if debug:
  print_battlefield(battlefield, units)

echo "Part 1: ", roundNum * sum_hp(units)
