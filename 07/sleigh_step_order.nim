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

# find the order and time required for the tasks if working on more than one task at a time
proc find_concurrent_time(instr: var Table[char,set[char]], num_workers: int, base_task_time: int): (string, int) =
  var
    working_tasks = initTable[char,int]()
    current_time = -1
    instruction_order: string

  # continue this process until no instructions are left
  # or worker's queue is empty
  while instr.len() > 0 or working_tasks.len() > 0:
    block steps:
      # increment the time on each loop
      current_time.inc()
      # check if any tasks are complete
      for task, done_time in working_tasks:
        if done_time == current_time:
          # clear this task from the queue
          working_tasks.del(task)
          # add it to our final string
          instruction_order.add(task)
          # remove it from other step's prerequisites
          instr = instr.filter_steps(task)
      # go through instructions in alphabetical order
      for step, prerequisites in instr:
        # if this step does not need any more steps first
        # and we have a spare worker
        if prerequisites == {} and working_tasks.len() < num_workers:
          # remove it from future consideration
          instr.del(step)
          # add it to the workers queue with the time it will be complete at
          # higher characters take more time ie. A=1 .. Z=26
          working_tasks[step] = current_time + base_task_time + (step.int - 64)
  return (instruction_order, current_time)

var
  instructions: Table[char,set[char]]
  order: string
  conc_order: string
  time: int
  workers = 5
  task_time = 60

instructions = read_file("input.txt")

order = find_order(instructions)

instructions = read_file("input.txt")

(conc_order, time) = instructions.find_concurrent_time(workers, task_time)

echo "Part 1: ", order
echo "Part 2: ", time
