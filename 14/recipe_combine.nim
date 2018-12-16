import strutils

type
  Recipes = seq[int]

var
  all_recipies: Recipes
  limit = 880751
  first_elf = 0
  second_elf = 1
  scores: string

# initial recipes
all_recipies.add(3)
all_recipies.add(7)

# keep making recipes until we have at least 10 more than the limit
while all_recipies.len() < limit + 10:
  var
    total = all_recipies[first_elf] + all_recipies[second_elf]
  # if total is 2 digits, only possible option is 1 in the tens position
  if total > 9:
    all_recipies.add(1)
  # add the units
  all_recipies.add(total mod 10)
  # get new elf positions
  first_elf = (first_elf + 1 + all_recipies[first_elf]) mod all_recipies.len()
  second_elf = (second_elf + 1 + all_recipies[second_elf]) mod all_recipies.len()

# append all the required scores together
for i in limit ..< limit + 10:
  scores.add(all_recipies[i].intToStr())

echo "Part 1: ", scores