import strutils

var
  num_twos: int
  num_threes: int
  exactly_two: bool
  exactly_three: bool
  count_chars: int

num_twos = 0
num_threes = 0

for line in lines "input.txt":
  exactly_two = false
  exactly_three = false
  for character in line:
    count_chars = count(line,character)
    if count_chars == 2:
      exactly_two = true
    elif count_chars == 3:
      exactly_three = true
    #once we find exactly two and three matches
    #we can stop searching on this line
    if exactly_two and exactly_three:
      break
  if exactly_two:
    num_twos += 1
  if exactly_three:
    num_threes += 1
  
echo num_twos * num_threes