import strutils

proc sumLines(f_name: string): int = 
  for line in lines f_name:
    result += parseInt(line)

echo sumLines("input.txt")
