import sequtils, strutils, re

type
  Registers = seq[int]
  Instruction = seq[int]
  Sample = ref object
    before: Registers
    instruction: Instruction
    after: Registers
    possible_codes: int

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

all_samples = build_samples("input.txt")

# try every instruction on every sample
for s in all_samples:
  if s.before.addr(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.addi(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.mulr(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.muli(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.banr(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.bani(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.borr(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.bori(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.setr(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.seti(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.gtir(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.gtri(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.gtrr(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.eqir(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.eqri(s.instruction) == s.after:
    s.possible_codes.inc()
  if s.before.eqrr(s.instruction) == s.after:
    s.possible_codes.inc()
  # count how many possible opcodes there are for each
  if s.possible_codes >= 3:
    possible_count.inc()

echo "Part 1: ", possible_count