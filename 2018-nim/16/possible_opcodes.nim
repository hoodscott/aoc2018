import sequtils, strutils, re

# part 1 - for s in all_samples:
# given a list of sets of samples (register_before, instruction, register_after)
# where registers are integer arrays 4 long and an instruction is an opcode
# (as integer), two inputs, and one output.  return the number of samples that
# behave the same for three or more multiple opcodes

# part 2 - for line in lines "input_program.txt":
# with above input, find out what opcode corresponds to sample integer and
# execute progam according to those rules.  then return the value in register 0
# (think I manually mapped int to opcode by looking at output of part 1 and
# incrementally matching opcodes)

type
  Registers = seq[int]
  Instruction = seq[int]
  Sample = ref object
    before: Registers
    instruction: Instruction
    after: Registers
    possible_codes: seq[string]

proc build_samples(f_name: string): seq[Sample] =
  var
    count = -1
    current_sample: Sample

  # go through the file to get the samples
  for line in lines f_name:
    count.inc()
    case count mod 4:
      of 0:
        current_sample = Sample(before: line.findAll(re"-?\d+").map(parseInt))
      of 1:
        current_sample.instruction = line.findAll(re"-?\d+").map(parseInt)
      of 2:
        current_sample.after = line.findAll(re"-?\d+").map(parseInt)
        result.add(current_sample)
      of 3:
        continue
      else:
        continue

# stores into register C the result of adding register A and register B
proc addr(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr[3]] = before[instr[1]] + before[instr[2]]

# stores into register C the result of adding register A and value B.
proc addi(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr[3]] = before[instr[1]] + instr[2]

# stores into register C the result of multiplying register A and register B.
proc mulr(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr[3]] = before[instr[1]] * before[instr[2]]

# stores into register C the result of multiplying register A and value B.
proc muli(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr[3]] = before[instr[1]] * instr[2]

# stores into register C the result of the bitwise AND of register A and register B.
proc banr(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr[3]] = before[instr[1]] and before[instr[2]]

# stores into register C the result of the bitwise AND of register A and value B.
proc bani(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr[3]] = before[instr[1]] and instr[2]

# stores into register C the result of the bitwise OR of register A and register B.
proc borr(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr[3]] = before[instr[1]] or before[instr[2]]

# stores into register C the result of the bitwise OR of register A and value B.
proc bori(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr[3]] = before[instr[1]] or instr[2]

# copies the contents of register A into register C. (Input B is ignored.)
proc setr(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr[3]] = before[instr[1]]

# stores value A into register C. (Input B is ignored.)
proc seti(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr[3]] = instr[1]

# sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
proc gtir(before: Registers, instr: Instruction): Registers =
  result = before
  if instr[1] > before[instr[2]]:
    result[instr[3]] = 1
  else:
    result[instr[3]] = 0

#  sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
proc gtri(before: Registers, instr: Instruction): Registers =
  result = before
  if before[instr[1]] > instr[2]:
    result[instr[3]] = 1
  else:
    result[instr[3]] = 0

# sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
proc gtrr(before: Registers, instr: Instruction): Registers =
  result = before
  if before[instr[1]] > before[instr[2]]:
    result[instr[3]] = 1
  else:
    result[instr[3]] = 0

# sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
proc eqir(before: Registers, instr: Instruction): Registers =
  result = before
  if instr[1] == before[instr[2]]:
    result[instr[3]] = 1
  else:
    result[instr[3]] = 0

# sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
proc eqri(before: Registers, instr: Instruction): Registers =
  result = before
  if before[instr[1]] == instr[2]:
    result[instr[3]] = 1
  else:
    result[instr[3]] = 0

# sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
proc eqrr(before: Registers, instr: Instruction): Registers =
  result = before
  if before[instr[1]] == before[instr[2]]:
    result[instr[3]] = 1
  else:
    result[instr[3]] = 0

var
  all_samples: seq[Sample]
  possible_count: int
  reg: Registers = newSeq[int](4)

all_samples = build_samples("input_samples.txt")

# try every instruction on every sample
for s in all_samples:
  # 9 == addr
  if s.before.addr(s.instruction) == s.after:
    s.possible_codes.add("addr")
  # 6 == addi
  if s.before.addi(s.instruction) == s.after:
    s.possible_codes.add("addi")
  # 8 == mulr
  if s.before.mulr(s.instruction) == s.after:
    s.possible_codes.add("mulr")
  # 0 == muli
  if s.before.muli(s.instruction) == s.after:
    s.possible_codes.add("muli")
  # 14
  if s.before.banr(s.instruction) == s.after:
    s.possible_codes.add("banr")
  # 11
  if s.before.bani(s.instruction) == s.after:
    s.possible_codes.add("bani")
  # 1 == borr
  if s.before.borr(s.instruction) == s.after:
    s.possible_codes.add("borr")
  # 10 == bori
  if s.before.bori(s.instruction) == s.after:
    s.possible_codes.add("bori")
  # 7
  if s.before.setr(s.instruction) == s.after:
    s.possible_codes.add("setr")
  # 12 = seti
  if s.before.seti(s.instruction) == s.after:
    s.possible_codes.add("seti")
  # 15
  if s.before.gtir(s.instruction) == s.after:
    s.possible_codes.add("gtir")
  # 2 == gtri
  if s.before.gtri(s.instruction) == s.after:
    s.possible_codes.add("gtri")
  # 4
  if s.before.gtrr(s.instruction) == s.after:
    s.possible_codes.add("gtrr")
  # 5
  if s.before.eqir(s.instruction) == s.after:
    s.possible_codes.add("eqir")
  # 3
  if s.before.eqri(s.instruction) == s.after:
    s.possible_codes.add("eqri")
  # 13
  if s.before.eqrr(s.instruction) == s.after:
    s.possible_codes.add("eqrr")
  # does this behave the same for more than three opcodes?
  if s.possible_codes.len() >= 3:
    possible_count.inc()

echo "Part 1: ", possible_count

# read the program
for line in lines "input_program.txt":
  var
    instr: Instruction = line.findAll(re"-?\d+").map(parseInt)
  # perform the correct operation
  case instr[0]:
    of 0:
      reg = reg.muli(instr)
    of 1:
      reg = reg.borr(instr)
    of 2:
      reg = reg.gtri(instr)
    of 3:
      reg = reg.eqri(instr)
    of 4:
      reg = reg.gtrr(instr)
    of 5:
      reg = reg.eqir(instr)
    of 6:
      reg = reg.addi(instr)
    of 7:
      reg = reg.setr(instr)
    of 8:
      reg = reg.mulr(instr)
    of 9:
      reg = reg.addr(instr)
    of 10:
      reg = reg.bori(instr)
    of 11:
      reg = reg.bani(instr)
    of 12:
      reg = reg.seti(instr)
    of 13:
      reg = reg.eqrr(instr)
    of 14:
      reg = reg.banr(instr)
    of 15:
      reg = reg.gtir(instr)
    else:
      echo "continue"
      continue

echo "Part 2: ", reg[0]
