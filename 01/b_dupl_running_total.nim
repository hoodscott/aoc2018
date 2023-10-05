import strutils
import sets

# given file of positive or negative numbers
# return the first time the same running total appears twice

proc first_duplicate_total (f_name: string): int =
  var running_total = initHashSet[int]()
  for line in lines f_name:
    result += parseInt(line)
    # we have found a repeated total so can leave early
    if running_total.contains(result):
      return result
    # keep track of all previous frequencies
    else:
      running_total.incl(result)

echo first_duplicate_total("input.txt")
