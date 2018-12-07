import tables, strutils

# read list of instructions into a table
proc read_file(f_name: string): Table[char,set[char]] =
  result = initTable[char,set[char]]()
  for line in lines f_name:
    var
      finish_letter = line.split()[1][0]
      before_letter = line.split()[7][0]
    if not result.contains(before_letter):
      result[before_letter] = {}
    if not result.contains(finish_letter):
      result[finish_letter] = {}
    result[before_letter].incl(finish_letter)

# remove completed steps from future prerequisites
proc filter_steps(instr: var Table[char,set[char]], filter: char): Table[char,set[char]] = 
  result = instr
  for k in result.keys():
    result[k].excl(filter)

# find the order of tasks
proc find_order(instr: var Table[char,set[char]]): string = 
  # continue this process until no instructions are left
  while instr.len > 0:
    block steps:
      # go through instructions in alphabetical order
      for step, prerequisites in instr:
        # if this step does not need any more steps first
        if prerequisites == {}:
          # add it to our final string
          result.add(step)
          # remove it from other step's prerequisites
          instr = instr.filter_steps(step)
          # remove it from future consideration
          instr.del(step)
          # start the loop from the beginning again
          break steps

var
  instructions: Table[char,set[char]]
  order: string

instructions = read_file("input.txt")

order = instructions.find_order()

echo "Part 1: ", order
