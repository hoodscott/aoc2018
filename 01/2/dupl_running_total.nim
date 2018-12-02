import strutils
import sets

var
  sum: int
  running_total = initSet[int]()

sum = 0
running_total.incl(sum)

block infinite_loop:
  while true:
    for line in lines "input.txt":
      sum += parseInt(line)
      if contains(running_total,sum):
        echo sum
        break infinite_loop;
      else:
        running_total.incl(sum)

#for value in running_total.items:
#  echo "Got ", value
