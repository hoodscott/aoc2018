import strutils, sequtils

# part 1 - for in inner
# starting with a circle containing just marble 0 (current), insert next numbered
# marble between marbles that are 1 and 2 away in clockwise direction.  inserted
# marble becomes new current. if marbe to be placed is multiple of 23, it is
# instead added to player score, the marble 7 away from current in counter-clockwise
# direction is removed and added to player score, and new current is one marble
# clockwise of removed marble.
# game repeats until final score matches that in input file (number of players
# also found in input)

# part 2 - for in outer
# same as above but what if the last marble's score was 100 times larger

type Marble = ref object of RootObj
  value*: int
  prev*: Marble
  next*: Marble

# read file to get game parameters
proc get_game_params(f_name: string): (int, int) =
  var
    line = read_file(f_name).split()
  return (line[0].parseInt(), line[6].parseInt())

# get the index for the current player
proc get_current_player(m: int, total: int): int =
  result = m mod total
  if result == 0:
    result = total

# remove this marble from the double linked list
proc delete_marble(m: Marble): Marble =
  # link previous marble to next marble
  m.prev.next = m.next
  # link next marble to previous marble
  m.next.prev = m.prev
  # return next marble
  return m.next

# add this marble to the double linked list
proc insert_marble(m: Marble, v: int): Marble =
  var
    new_marble = Marble(value: v)
    next_marble = m.next
  # link current marble to new marble
  m.next = new_marble
  new_marble.prev = m
  # link old next marble to new marble
  next_marble.prev = new_marble
  new_marble.next = next_marble
  # return new marble
  return new_marble

var
  current_marble: Marble
  scores: seq[int]
  num_players: int
  num_marbles: int
  num_marbles_big: int
  current_player: int

# read file to get game parameters
(num_players, num_marbles) = get_game_params("input.txt")

# initialise scores sequence with one entry per player
scores = newSeqWith(num_players, 0)

# game begins with just the initial zero-value marble
current_marble = Marble(value: 0)

# double-link the marble to itself
current_marble.next = current_marble
current_marble.prev = current_marble

# make last marble 100 times larger
num_marbles_big = num_marbles * 100

# loop from 1 to number of marbles
for marble in 1 .. num_marbles_big:
  if marble mod 23 == 0:
    # get the current player
    current_player = get_current_player(marble, num_players)
    # change current marble to 7th previous
    for i in 1 .. 7:
      current_marble = current_marble.prev
    # add the current marble and the marble at this position to their score (zero-indexed)
    scores[current_player - 1] += marble + current_marble.value
    # remove this marble from the board
    current_marble = current_marble.delete_marble()
  else:
    # new current position is one to the right
    current_marble = current_marble.next
    # place the new marble there
    current_marble = current_marble.insert_marble(marble)
  # When we reach the normal end, report the first highest score
  if marble == num_marbles:
    echo "Part 1: ", scores.max()

# find the biggest score now the game has been fully played
echo "Part 2: ", scores.max()
