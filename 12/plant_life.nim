import sequtils, strutils

type
  Rule = ref object
    left_2: bool
    left_1: bool
    centre: bool
    right_1: bool
    right_2: bool
    result: bool
  Pot = ref object
    value: int
    plant: bool
    left: Pot
    right: Pot

proc create_plant_row(f_name: string): (Pot, seq[Rule]) =
  var
    first_pot: Pot
    prev_pot: Pot
    read_rules: seq[Rule]
  var
    line_no = 0
  for line in lines f_name:
    # read the initial state on the first line
    if line_no == 0:
      for index, letter in line.split()[2]:
        if index == 0:
          first_pot = Pot(value: index, plant: letter == '#')
          prev_pot = first_pot
        else:
          prev_pot.right = Pot(value: index, plant: letter == '#', left: prev_pot)
          prev_pot = prev_pot.right
    # read the rules in the remainder of the file
    elif line_no > 1:
      var 
        line_seq = line.split()
      read_rules.add(Rule(left_2: line_seq[0][0] == '#', left_1: line_seq[0][1] == '#', centre: line_seq[0][2] == '#',
                          right_1: line_seq[0][3] == '#', right_2: line_seq[0][4] == '#', result: line_seq[2][0] == '#'))
    line_no.inc()
  return (first_pot, read_rules)

proc add_space(pot: Pot): Pot =
  var
    pot_pointer = pot
  # find first pot with a plant
  while not pot_pointer.plant:
    pot_pointer = pot_pointer.right
  
  # go left four pots
  for index in 1 .. 4:
    # create one if necessary
    if pot_pointer.left == nil:
      pot_pointer.left = Pot(value: pot_pointer.value - 1, plant: false, right: pot_pointer)
    pot_pointer = pot_pointer.left
  
  # lose the front
  pot_pointer.left = nil
  result = pot_pointer

  # go through to the end
  while pot_pointer.right != nil:
    pot_pointer = pot_pointer.right
  
  # find last pot with a plant
  while not pot_pointer.plant:
    pot_pointer = pot_pointer.left

  # go right four pots
  for index in 1 .. 4:
    # create one if necessary
    if pot_pointer.right == nil:
      pot_pointer.right = Pot(value: pot_pointer.value + 1, plant: false, left: pot_pointer)
    pot_pointer = pot_pointer.right

proc print_pots(pot: Pot): string =
  var
    current_pot = pot
  while current_pot != nil:
    if current_pot.plant:
      result.add('#')
    else:
      result.add('.')
    # result.add(current_pot.value.intToStr())
    current_pot = current_pot.right

proc future_plant(pot: Pot, rules: seq[Rule]): bool =
  #check all rules
  for rule in rules:
    if rule.left_2 == pot.left.left.plant and rule.left_1 == pot.left.plant and
        rule.centre == pot.plant and rule.right_1 == pot.right.plant and
        rule.right_2 == pot.right.right.plant and rule.result:
      return true
  return false

proc tick(old_pointer: var Pot, rules: seq[Rule]): Pot =
  var
    new_pointer: Pot
  # skip the first two pots
  old_pointer = old_pointer.right.right
  # loop through pots until there are only two left
  while old_pointer.right.right != nil:
    # create the first pot on the first loop
    if new_pointer == nil:
      new_pointer = Pot(value: old_pointer.value, plant: old_pointer.future_plant(rules))
      old_pointer = old_pointer.right
      result = new_pointer
    # otherwise add the next pot on
    else:
      new_pointer.right = Pot(value: old_pointer.value, plant: old_pointer.future_plant(rules), left: new_pointer)
      new_pointer = new_pointer.right
      old_pointer = old_pointer.right

proc sum(pot: Pot): int =
  var
    pot_pointer = pot
  while pot_pointer != nil:
    if pot_pointer.plant:
      result += pot_pointer.value
    pot_pointer = pot_pointer.right
var
  leftmost_pot: Pot
  rules: seq[Rule]
  current_pot: Pot

(leftmost_pot, rules) = create_plant_row("input.txt")

leftmost_pot = leftmost_pot.add_space()

echo "Gen 0: ", leftmost_pot.print_pots()

for gen in 1 .. 20:
  leftmost_pot = leftmost_pot.tick(rules)
  leftmost_pot = leftmost_pot.add_space()
  echo "Gen ", gen, ": ", leftmost_pot.print_pots()

echo "Part 1: ", leftmost_pot.sum()

for gen in 21 .. 50000000000:
  leftmost_pot = leftmost_pot.tick(rules)
  leftmost_pot = leftmost_pot.add_space()
  if gen mod 10000 == 0:
    echo "Gen ", gen, ": ", leftmost_pot.print_pots()
    echo "Running Total: ", leftmost_pot.sum()

echo "Part 2: ", leftmost_pot.sum()