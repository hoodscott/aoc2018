import strutils

# part 1 - read_file
# given input of string of upper and lowercase letters where adjacent letters
# in opposite cases combine/reduce to leave nothing, simulate the reduction and
# return the length of the reduced string

# part 2 - process*26 then find
# given same as above, see which letter of the alphabet gives the lowest resulting
# polymer when its upper/lowercase letters are removed before processing.  return
# the shortest length


proc read_file(f_name: string): seq[char] =
  var
    length: int
  for line in lines f_name:
    for unit in line:
      # add each character to the sequence
      result.add(unit)
      length = result.len
      # if we have two things in the sequence
      # and the last two characters are 32 bytes away in ASCII
      # (upper and lowercase version of same character)
      while length > 1 and (result[length - 1].int() - result[length - 2].int()).abs() == 32:
        # delete these two units
        result.del(length - 1)
        result.del(length - 2)
        length = result.len

proc filter_polymer(poly: seq[char], filter: int): seq[char] =
  var
    length: int
  for unit in poly:
    if unit.toUpperAscii().int() == filter:
      continue
    result.add(unit)
    length = result.len
    # if we have two things in the sequence
    # and the last two characters are 32 bytes away in ASCII
    # (upper and lowercase version of same character)
    while length > 1 and (result[length - 1].int() - result[length - 2].int()).abs() == 32:
      # reduce these two units
      result.del(length - 1)
      result.del(length - 2)
      length = result.len

var
  polymer: seq[char]
  polymers: seq[seq[char]]
  shortest_polymer: char
  shortest_polymer_length = high(int)

# read file into a sequence
polymer = read_file("input.txt")

# Length of the reduced polymer
echo "Part 1: ", polymer.len

# filter out each letter of the alphabet from the initial polymer
for c in 65 .. 90:
  polymers.add(polymer.filter_polymer(c))

# find the smallest polymer chain length
for index, poly in polymers:
  if poly.len() < shortest_polymer_length:
    shortest_polymer_length = poly.len()
    shortest_polymer = (index + 65).chr()

# Length of the shortest possible polymer + which letter should be filtered
echo "Part 2: ", shortest_polymer_length, ", ", shortest_polymer
