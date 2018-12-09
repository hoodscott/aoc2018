import strutils, sequtils

# read file to get game parameters
proc get_game_params(f_name: string): (int, int) =
  var
    line = read_file(f_name).split()
  return (line[0].parseInt(), line[6].parseInt())

proc get_current_player(m: int, total: int): int =
  result = m mod total
  if result == 0:
    result = total

var
  game: seq[int]
  scores: seq[int]
  num_players: int
  num_marbles: int
  current_position: int
  current_player: int

# read file to get game parameters
(num_players, num_marbles) = get_game_params("input.txt")

# initialise scores sequence with one entry per player
scores = newSeqWith(num_players,0)

# game begins with just the initial marble
game.add(0)

# loop from 1 to number of marbles
for marble in 1 .. num_marbles:
  if marble mod 23 == 0:
    # get the current player
    current_player = get_current_player(marble, num_players)
    # change current marble to 7th previous in counter-clockwise order
    # we add the length of the current board
    # so we dont need to worry about negative modulo calculations
    current_position = (current_position + game.len() - 7) mod game.len()
    # add the current marble and the marble at this position to their score (zero-indexed)
    scores[current_player - 1] += marble + game[current_position]
    # remove this marble from the board
    game.delete(current_position)
  else:
    # calculate the new current position
    # (one right of the next marble in clockwise order)
    current_position = (current_position + 1) mod game.len() + 1
    # place the new marble there
    game.insert(marble,current_position)

# find the biggest score
echo "Part 1: ", scores.max()
