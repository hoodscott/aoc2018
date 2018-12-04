import strutils
import sets

proc first_duplicate_total (f_name: string): int = 
  var
    running_total = initSet[int]()
  # loop infinitely around the file
  while true:
    for line in lines f_name:
      result += parseInt(line)
      if running_total.contains(result):
        # when the first duplicate total is reached
        return result
      else:
        # keep track of all previous totals
        running_total.incl(result)

echo first_duplicate_total("input.txt")
