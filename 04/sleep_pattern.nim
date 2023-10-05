import algorithm
import tables
import strutils

# part 1 - build_guard_pattern sleepiest_guard_minute
# given list of timestamps and actions (guard begins shift/falls asleep/wakes up)
# not necessarily in chronological order, find the guard who has spent the most
# time asleep and return the minute that they spend asleep most often.

# part 2 - sleepiest_minute
# given list of timestamps and actions (guard begins shift/falls asleep/wakes up)
# not necessarily in chronological order, find the guard who has spent the same
# minute asleep more times that any other guard has spent a minute asleep.
# return the guard id * their most asleep minute

type
  SleepPattern = ref object of RootObj
    total_sleep*: int
    sleep_freq*: seq[int]

proc read_file(f_name: string): seq[string] =
  for line in lines f_name:
    result.add(line)

# builds the cumulative patter for each guard
# keep track of the sleepiest guard while building this
proc build_guard_pattern(guard_records: seq[string]): (Table[int, SleepPattern], int) =
  var
    current_guard: int
    sleep_start: int
    sleep_end: int
    sleepiest_guard_id: int
    sleepiest_guard_total = -1
    guard_patterns = initTable[int, SleepPattern]()
  # go back through records in chronological order
  for record in guard_records:
    # split logic depending on record type
    case record.split()[2]
    of "Guard":
      # if new guard - change pointer and create object if necessary
      current_guard = record.split()[3].replace("#").parseInt()
      if not guard_patterns.hasKey(current_guard):
        guard_patterns[current_guard] = SleepPattern(total_sleep: 0,
            sleep_freq: newSeq[int](60))
    of "falls":
      # if sleep - parse start time from string and remember
      sleep_start = record.replace("]").split({' ', ':'})[2].parseInt()
    of "wakes":
      # if wakeup - record sleep duration and times
      sleep_end = record.replace("]").split({' ', ':'})[2].parseInt()
      for min in sleep_start .. sleep_end - 1:
        guard_patterns[current_guard].total_sleep.inc()
        guard_patterns[current_guard].sleep_freq[min].inc()
      # keep track of the guard with the most minutes slept
      if guard_patterns[current_guard].total_sleep > sleepiest_guard_total:
        sleepiest_guard_total = guard_patterns[current_guard].total_sleep
        sleepiest_guard_id = current_guard
  return (guard_patterns, sleepiest_guard_id)

proc sleepiest_guard_minute(patterns: Table[int, SleepPattern],
    guard_id: int): int =
  var
    sleepiest_minute: int
    sleepiest_frequency = -1
  for index, minute_total in patterns[guard_id].sleep_freq:
    if minute_total > sleepiest_frequency:
      sleepiest_frequency = minute_total
      sleepiest_minute = index
  return sleepiest_minute

proc sleepiest_minute(patterns: Table[int, SleepPattern]): int =
  var
    sleepiest_minute_count: int
    sleepiest_minute: int
    sleepiest_minute_guard: int
  for guard_id, guard_pattern in patterns:
    for index, minute in guard_pattern.sleep_freq:
      if minute > sleepiest_minute_count:
        sleepiest_minute_count = minute
        sleepiest_minute = index
        sleepiest_minute_guard = guard_id
  return sleepiest_minute_guard * sleepiest_minute

var
  records: seq[string]
  sleepiest_guard: int
  guard_patterns: Table[int, SleepPattern]

# read file into a sequence
records = read_file("input.txt")

# we can sort these into datetime order by just using
# basic string comparison as they are in a nice format
records.sort(system.cmp[string])

# process the records to build a pattern for each guard
(guard_patterns, sleepiest_guard) = build_guard_pattern(records)

# find the most likely minute for the sleepiest guard to be asleep
echo "Part 1: ", sleepiest_guard * guard_patterns.sleepiest_guard_minute(sleepiest_guard)

# find the most likely minute for any guard to be asleep
echo "Part 2: ", guard_patterns.sleepiest_minute()
