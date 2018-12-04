import strutils

var
  sum: int

sum = 0

for line in lines "input.txt":
  sum += parseInt(line)

echo sum