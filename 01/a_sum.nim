import strutils

# given file of positive or negative numbers,
# find the total after summing all numbers

proc sumLines(f_name: string): int =
  for line in lines f_name:
    result += parseInt(line)

echo sumLines("input.txt")
