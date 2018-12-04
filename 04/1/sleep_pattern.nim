import algorithm
import tables
import strutils

type
  SleepPattern = ref object of RootObj
    total_sleep*: int
    sleep_freq*: seq[int]

var
  records: seq[string]
  guard_patterns = initTable[int, SleepPattern]()
  current_guard: int
  sleep_start: int
  sleep_end: int
  sleepiest_guard: int
  sleepiest_guard_total = -1
  sleepiest_minute: int
  sleepiest_minute_total = -1

# get records into a sequence
for line in lines "input.txt":
  records.add(line)

# we can sort these into datetime order by just using
# basic string comparison as they are in a nice format
records.sort(system.cmp[string])

# go back through records in chronological order
for record in records:
  # split logic depending on record type
  case record.split()[2]
  of "Guard":
    # if new guard - change pointer and create object if necessary
    current_guard = record.split()[3].replace("#").parseInt()
    if not guard_patterns.hasKey(current_guard):
      guard_patterns.add(current_guard, SleepPattern(total_sleep: 0, sleep_freq: newSeq[int](60)))
  of "falls":
    # if sleep - parse start time from string and remember
    sleep_start = record.replace("]").split({' ', ':'})[2].parseInt()
  of "wakes":
    # if wakeup - record sleep duration and times
    sleep_end = record.replace("]").split({' ', ':'})[2].parseInt()
    for min in sleep_start .. sleep_end - 1:
      guard_patterns[current_guard].total_sleep += 1
      guard_patterns[current_guard].sleep_freq[min] += 1
    # keep track of the guard with the most minutes slept
    if guard_patterns[current_guard].total_sleep > sleepiest_guard_total:
      sleepiest_guard_total = guard_patterns[current_guard].total_sleep
      sleepiest_guard = current_guard

# find the most likely minute for the sleepiest guard to be asleep
for index, minute_total in guard_patterns[sleepiest_guard].sleep_freq:
  if minute_total > sleepiest_minute_total:
    sleepiest_minute_total = minute_total
    sleepiest_minute = index

echo "sleepiest guard: ", sleepiest_guard
echo "sleepiest min: ", sleepiest_minute
echo "sleepiest pattern: ", guard_patterns[sleepiest_guard].sleep_freq
echo "sleepiest total: ", guard_patterns[sleepiest_guard].total_sleep

echo "Part 1: ", sleepiest_guard * sleepiest_minute