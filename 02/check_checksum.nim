import strutils

proc calc_checksum (f_name: string): int = 
  var
    num_twos = 0
    num_threes = 0

  for line in lines f_name:
    var
      exactly_two = false
      exactly_three = false
    for character in line:
      var
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
      num_twos.inc()
    if exactly_three:
      num_threes.inc()
  
  return num_twos * num_threes

echo calc_checksum("input.txt")