import sequtils, strutils, re

type
  Registers = seq[int]
  Instruction = ref object
    opcode: string
    operands: seq[int]
  Program = seq[Instruction]

# read file into a sequence and find the instruction pointer index
proc read_program(f_name: string): (int, Program) =
  for line in lines f_name:
    if line.split().len() == 2:
      result[0] = line.split()[1].parseInt()
    else:
      result[1].add(Instruction(opcode: line.split()[0], operands: line.findAll(re"-?\d+").map(parseInt)))

# stores into register C the result of adding register A and register B
proc addr(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr.operands[2]] = before[instr.operands[0]] + before[instr.operands[1]]

# stores into register C the result of adding register A and value B.
proc addi(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr.operands[2]] = before[instr.operands[0]] + instr.operands[1]

# stores into register C the result of multiplying register A and register B.
proc mulr(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr.operands[2]] = before[instr.operands[0]] * before[instr.operands[1]]

# stores into register C the result of multiplying register A and value B.
proc muli(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr.operands[2]] = before[instr.operands[0]] * instr.operands[1]
  
# stores into register C the result of the bitwise AND of register A and register B.
proc banr(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr.operands[2]] = before[instr.operands[0]] and before[instr.operands[1]]

# stores into register C the result of the bitwise AND of register A and value B.
proc bani(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr.operands[2]] = before[instr.operands[0]] and instr.operands[1]
  
# stores into register C the result of the bitwise OR of register A and register B.
proc borr(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr.operands[2]] = before[instr.operands[0]] or before[instr.operands[1]]

# stores into register C the result of the bitwise OR of register A and value B.
proc bori(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr.operands[2]] = before[instr.operands[0]] or instr.operands[1]
  
# copies the contents of register A into register C. (Input B is ignored.)
proc setr(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr.operands[2]] = before[instr.operands[0]]

# stores value A into register C. (Input B is ignored.)
proc seti(before: Registers, instr: Instruction): Registers =
  result = before
  result[instr.operands[2]] = instr.operands[0]
  
# sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
proc gtir(before: Registers, instr: Instruction): Registers =
  result = before
  if instr.operands[0] > before[instr.operands[1]]:
    result[instr.operands[2]] = 1
  else:
    result[instr.operands[2]] = 0

#  sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
proc gtri(before: Registers, instr: Instruction): Registers =
  result = before
  if before[instr.operands[0]] > instr.operands[1]:
    result[instr.operands[2]] = 1
  else:
    result[instr.operands[2]] = 0

# sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
proc gtrr(before: Registers, instr: Instruction): Registers =
  result = before
  if before[instr.operands[0]] > before[instr.operands[1]]:
    result[instr.operands[2]] = 1
  else:
    result[instr.operands[2]] = 0

# sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
proc eqir(before: Registers, instr: Instruction): Registers =
  result = before
  if instr.operands[0] == before[instr.operands[1]]:
    result[instr.operands[2]] = 1
  else:
    result[instr.operands[2]] = 0

# sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
proc eqri(before: Registers, instr: Instruction): Registers =
  result = before
  if before[instr.operands[0]] == instr.operands[1]:
    result[instr.operands[2]] = 1
  else:
    result[instr.operands[2]] = 0

# sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
proc eqrr(before: Registers, instr: Instruction): Registers =
  result = before
  if before[instr.operands[0]] == before[instr.operands[1]]:
    result[instr.operands[2]] = 1
  else:
    result[instr.operands[2]] = 0

var
  prog: Program
  instr_pointer: int
  reg: Registers = newSeq[int](6)

(instr_pointer, prog) = read_program("input.txt")

echo instr_pointer

while reg[instr_pointer] < prog.len():
  var
    cur_instr = prog[reg[instr_pointer]]
  case cur_instr.opcode:
    of "muli":
      reg = reg.muli(cur_instr)
    of "borr":
      reg = reg.borr(cur_instr)
    of "gtri":
      reg = reg.gtri(cur_instr)
    of "eqri":
      reg = reg.eqri(cur_instr)
    of "gtrr":
      reg = reg.gtrr(cur_instr)
    of "eqir":
      reg = reg.eqir(cur_instr)
    of "addi":
      reg = reg.addi(cur_instr)
    of "setr":
      reg = reg.setr(cur_instr)
    of "mulr":
      reg = reg.mulr(cur_instr)
    of "addr":
      reg = reg.addr(cur_instr)
    of "bori":
      reg = reg.bori(cur_instr)
    of "bani":
      reg = reg.bani(cur_instr)
    of "seti":
      reg = reg.seti(cur_instr)
    of "eqrr":
      reg = reg.eqrr(cur_instr)
    of "banr":
      reg = reg.banr(cur_instr)
    of "gtir":
      reg = reg.gtir(cur_instr)
    else:
      echo "continue"
      continue
  reg[instr_pointer].inc()

echo "Part 1: ", reg[0]