# read units into a sequence
# if any uppercase and lowercase letters are adjacent,
# they are removed from the sequence
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

var
  polymer: seq[char]

# read file into a sequence
polymer = read_file("input.txt")

# Length of the reduced polymer
echo "Part 1: ", polymer.len
