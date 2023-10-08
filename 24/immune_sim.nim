import std/re, strutils, tables, heapqueue, sets

# part 1 - countRemainingUnits(armies)
# given an input detailing two armies, simulate the battle until one army
# has no unit left then report the number of units remaining in the winning
# army

# part 2 - simulateBinarySearch(..)
# with same input as above, try to find the smallest increase in Immune system
# attack power that allows them to win. report the number of units remaining in
# this scenario

type
  Army = enum
    Infection, Immune

  Group = object
    faction: Army
    units: int
    hp: int
    power: int
    typ: string
    initiative: int
    weaknesses: seq[string]
    immunities: seq[string]

  Selection = object
    id: int
    effectivePower: int
    initiative: int

  Attack = object
    attackerId: int
    defenderId: int
    initiative: int
    damage: int


# for heapqueue priority
proc `<`(a, b: Selection): bool =
  # order groups by decending effective power (num units in group * attackD),
  # break ties by higher initiative
  if a.effectivePower == b.effectivePower:
    a.initiative > b.initiative
  else:
    a.effectivePower > b.effectivePower

# for heapqueue priority
proc `<`(a, b: Attack): bool =
  # order by decreasing initiative
  a.initiative > b.initiative

proc readInput(fName: string, boost = 0): Table[int, Group] =
  var
    currentFaction: Army
    groupID: int
  result = initTable[int, Group]()

  for line in lines fName:
    if line.len == 0:
      continue
    if line == "Immune System:":
      currentFaction = Immune
      continue
    if line == "Infection:":
      currentFaction = Infection
      continue

    var
      details: array[5, string]
      weaknesses: array[1, string]
      immunities: array[1, string]

    if line.find(re"^(\d+).*?(\d+).*?(\d+) (\w+).*?(\d+)", details) >= 0:
      discard line.find(re"weak to ((?:\w+|, )+)", weaknesses)
      discard line.find(re"immune to ((?:\w+|, )+)", immunities)
      result[groupID] = Group(
        faction: currentFaction,
        units: parseInt(details[0]),
        hp: parseInt(details[1]),
        power: parseInt(details[2]) + (
          if currentFaction == Immune: boost else: 0),
        typ: details[3],
        initiative: parseInt(details[4]),
        weaknesses: weaknesses[0].split(", "),
        immunities: immunities[0].split(", ")
      )
      groupID.inc()


proc battleCanContinue(armies: Table[int, Group]): bool =
  var
    hasImmune = false
    hasInfection = false

  for arm in values(armies):
    case arm.faction:
      of Immune:
        hasImmune = true
      of Infection:
        hasInfection = true
    if hasImmune and hasInfection:
      return true

  return false


proc calcEP(group: Group): int =
  # effective power is number of units * attack power
  group.units * group.power


proc calcDamage(attacker, defender: Group, d = false): int =
  if attacker.typ in defender.immunities:
    result = 0
  else:
    result = calcEP(attacker)
    if d: echo attacker.units, " * ", attacker.power, " = ", result

    if attacker.typ in defender.weaknesses:
      if d: echo "doubled due to weakness"
      result *= 2


proc dealDamage(armies: var Table[int, Group], attack: Attack): bool =
  # damage dealt removes whole units (unit cannot have less than max hp)
  # need to recalc damage as attacker may have lost units since selection phase
  let kills = calcDamage(armies[attack.attackerId],
    armies[attack.defenderId], true) div armies[attack.defenderId].hp
  armies[attack.defenderId].units = armies[attack.defenderId].units - kills
  var imin = if armies[attack.attackerId].faction == Immune: "imu"
  else: "inf"
  echo ""
  echo attack.initiative, " ", attack.attackerId, "-", attack.defenderId, " ",
      imin, " ", calcDamage(armies[attack.attackerId],
    armies[attack.defenderId]), " ", kills
  result = kills > 0


proc countRemainingUnits(armies: Table[int, Group]): int =
  for arm in values(armies):
    result.inc(arm.units)


proc simulateBattle(armies: var Table[int, Group]): void =
  # continue battle while both armies still have groups
  while battleCanContinue(armies):
    # order groups by decreasing effective power, break ties on initiative
    var selectQ = initHeapQueue[Selection]()
    for id, group in pairs(armies):
      selectQ.push(Selection(id: id, effectivePower: calcEP(group),
          initiative: group.initiative))

    # select target for each group
    var
      attackQ = initHeapQueue[Attack]()
      targeted = initHashSet[int]()

    while selectQ.len > 0:
      let current = selectQ.pop()
      var
        maxDamage = 0
        maxEp = int.low
        maxInitiative = int.low
        targetId = -1

      for id, other in pairs(armies):
        # only target opposite faction
        if armies[current.id].faction == other.faction: continue
        # skip if this group has already been targeted this turn
        if id in targeted: continue

        # choose to attack enemy group where most damage would be done
        let damage = calcDamage(armies[current.id], other)
        if damage != 0:
          if damage > maxDamage:
            maxDamage = damage
            maxEp = calcEP(other)
            maxInitiative = other.initiative
            targetId = id
          elif damage == maxDamage:
            # if would deal same dmg to two groups
            # target largest effective power (num units in group * attackD),
            # break ties by largest initiative
            let maybeMaxEp = calcEP(other)
            if maybeMaxEp > maxEp or (maybeMaxEp == maxEp and other.initiative >
                maxInitiative):
              maxDamage = damage
              maxEp = maybeMaxEp
              maxInitiative = other.initiative
              targetId = id


      # if no targets then no attack
      if targetId >= 0:
        attackQ.push(Attack(attackerId: current.id, defenderId: targetId,
            initiative: current.initiative, damage: maxDamage))
        targeted.incl(targetId)
        # at end each group is attacking 0/1 group and being attacked by 0/1 group

    var anyDamageDealt = false
    while attackQ.len > 0:
      # attack in order of decreasing initiative
      let current = attackQ.pop()
      # groups killed in this round cannot attack
      if armies[current.attackerId].units > 0:
        let wasDamageDealt = dealDamage(armies, current)
        if wasDamageDealt:
          anyDamageDealt = true

    # if no damage was dealt this turn then we have a stalemate
    if not anyDamageDealt:
      break

    # at end of round remove the dead units
    var deadUnits: seq[int]
    for id, group in pairs(armies):
      if group.units <= 0:
        deadUnits.add(id)
    for id in deadUnits:
      armies.del(id)

    # echo armies
    # var x = stdin.readLine

    # echo "NEW ROUND"
    # for group in values(armies):
    #   if group.faction == Immune:
    #     echo group.units
    # var z = stdin.readLine()


proc isImmuneWin(armies: Table[int, Group]): bool =
  for group in values(armies):
    # all living remaining groups must be in the Immune faction to count as a win
    if group.hp > 0:
      if group.faction != Immune:
        return false
  return true


proc findUpperBound(filename: string): int =
  var boost = 1
  while true:
    var boostedArmies = readInput(filename)
    for group in mvalues(boostedArmies):
      if group.faction == Immune:
        group.power = group.power + boost
    boostedArmies.simulateBattle()

    if isImmuneWin(boostedArmies):
      return boost
    boost *= 2


proc simulateBinarySearch(filename: string, lower, upper: int): int =
  var
    left = lower
    right = upper


  while true:
    var
      mid = (right + left) div 2 # + 1 # important to take ceiling here
      boostedArmies = readInput(filename)

    for group in mvalues(boostedArmies):
      if group.faction == Immune:
        group.power = group.power + mid
    boostedArmies.simulateBattle()

    if isImmuneWin(boostedArmies):
      if right == left:
        echo boostedArmies
        return countRemainingUnits(boostedArmies)
      else:
        right = mid
    else:
      left = mid + 1



const filename = "input.txt"
  # var p1armies = readInput(filename)

  # p1armies.simulateBattle()

  # at end of game, print number of units in winning army
  # echo "Part 1: ", countRemainingUnits(p1armies)

  # do a specific boost
var boostedArmies = readInput(filename, 188)
  # for group in mvalues(boostedArmies):
  #   if group.faction == Immune:
  #     group.power = group.power + 188

boostedArmies.simulateBattle()
echo "simmed"
echo boostedArmies
echo countRemainingUnits(boostedArmies)


# # brute force
# var immuneWin = false
# var boost = 1
# while not immuneWin:
#   var boostedArmies = readInput(filename)

#   for group in mvalues(boostedArmies):
#     if group.faction == Immune:
#       group.power = group.power + boost
#   boostedArmies.simulateBattle()

#   immuneWin = isImmuneWin(boostedArmies)

#   if immuneWin:
#     echo "immune win ", boost
#     echo "count", countRemainingUnits(boostedArmies)
#   else:
#     echo "illnesswin ", boost
#     boost.inc()


# let upperBound = findUpperBound(filename)
# #echo upperBound
# echo "Part 2: ", simulateBinarySearch(filename, upperBound div 2, upperBound)
#979 too high
#965 too high
