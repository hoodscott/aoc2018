import strutils

# part 1 - while all_recipies.len() < limit + 10:
# two scores out of ten, add them together and then add the tens and units of
# the result to a list containing the initial two score.  then choose two new
# scores from this list by moving 1+score positions to the right of the current
# score indexes and repeat the process.  repeat this until there are <limit>
# scores plus 10 more results times and return the 10 result scores

# part 2 - block create_recipes:
# repeat the above process until the scores end in <input> then return the count
# of scores preceeding <input>

type
  Recipes = seq[int]

var
  all_recipies: Recipes
  running_recipes: Recipes
  limit = 880751
  str_limit = limit.intToStr()
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

# reset initial recipes and positions
first_elf = 0
second_elf = 1
running_recipes.add(3)
running_recipes.add(7)

block create_recipes:
  while true:
    var
      total = running_recipes[first_elf] + running_recipes[second_elf]
      tens: bool
      recipe_compare: string
    # if total is 2 digits, only possible option is 1 in the tens position
    if total > 9:
      running_recipes.add(1)
      tens = true
    # add the units
    running_recipes.add(total mod 10)
    # get new elf positions
    first_elf = (first_elf + 1 + running_recipes[
        first_elf]) mod running_recipes.len()
    second_elf = (second_elf + 1 + running_recipes[
        second_elf]) mod running_recipes.len()
    # check the recipes at the end
    if running_recipes.len() >= str_limit.len():
      for i in running_recipes.len() - str_limit.len() ..< running_recipes.len():
        recipe_compare.add(running_recipes[i].intToStr())
      # if the end of the running recipes matches the input, break out the loop and show the result
      if recipe_compare == str_limit:
        echo "Part 2: ", running_recipes.len() - str_limit.len()
        break create_recipes
      # if we added two recipes in this iteration, we need to check one in from the end aswell
      if tens and running_recipes.len() >= str_limit.len() + 1:
        recipe_compare = ""
        for i in running_recipes.len() - str_limit.len() - 1 ..<
            running_recipes.len() - 1:
          recipe_compare.add(running_recipes[i].intToStr())
        # if the end of the running recipes matches the input, break out the loop and show the result
        if recipe_compare == str_limit:
          echo "Part 2: ", running_recipes.len() - str_limit.len() - 1
          break create_recipes
