import sequtils, strutils

type
  WoodContents {.pure.} = enum
    open, trees, lumberyard
  Landscape = seq[seq[WoodContents]]

proc build_tracks(f_name: string): LandScape =
  var
    height: int
    width: int
    y: int
  
  # get the dimensions
  for line in lines f_name:
    width = line.len
    height.inc()

  result = newSeqWith(height, newSeq[WoodContents](width))

  # go back through the file to get the track and cart positions
  for line in lines f_name:
    for x, symbol in line:
      case symbol:
        of '#':
          result[y][x] = WoodContents.lumberyard
        of '|':
          result[y][x] = WoodContents.trees
        else:
          result[y][x] = WoodContents.open
    y.inc()

proc print_track(current_track: Landscape): void =
  for y, line in current_track:
    var
      printed_line: string
    for x, cell in line:
      case cell:
        of WoodContents.open:
          printed_line.add('.')
        of WoodContents.trees:
          printed_line.add('|')
        of WoodContents.lumberyard:
          printed_line.add('#')
    echo printed_line

proc tick(current_board: Landscape): Landscape =
  var
    height = current_board.len()
    width = current_board[0].len()
  
  # copy the current board and reset all acres
  result = newSeqWith(height, newSeq[WoodContents](width))
  
  for y, line in current_board:
    for x, cell in line:
      var
        wooded_count: int
        lumber_count: int
      if cell != WoodContents.trees:
        # if the area not wooded, count the surrounding wooded acres
        # top left
        if x != 0 and y != 0:
          if current_board[y - 1][x - 1] == WoodContents.trees:
            wooded_count.inc()
        # top
        if y != 0:
          if current_board[y - 1][x] == WoodContents.trees:
            wooded_count.inc()
        # top right
        if x < width - 1 and y != 0:
          if current_board[y - 1][x + 1] == WoodContents.trees:
            wooded_count.inc()
        # left
        if x != 0:
          if current_board[y][x - 1] == WoodContents.trees:
            wooded_count.inc()
        # right
        if x < width - 1:
          if current_board[y][x + 1] == WoodContents.trees:
            wooded_count.inc()
        # bottom left
        if x != 0 and y < height - 1:
          if current_board[y + 1][x - 1] == WoodContents.trees:
            wooded_count.inc()
        # bottom
        if y < height - 1:
          if current_board[y + 1][x] == WoodContents.trees:
            wooded_count.inc()
        # bottom right
        if x < width - 1 and y < height - 1:
          if current_board[y + 1][x + 1] == WoodContents.trees:
            wooded_count.inc()
      if cell != WoodContents.open:
        # if the area is not open, count the surrounding lumberyards
        # top left
        if x != 0 and y != 0:
          if current_board[y - 1][x - 1] == WoodContents.lumberyard:
            lumber_count.inc()
        # top
        if y != 0:
          if current_board[y - 1][x] == WoodContents.lumberyard:
            lumber_count.inc()
        # top right
        if x < width - 1 and y != 0:
          if current_board[y - 1][x + 1] == WoodContents.lumberyard:
            lumber_count.inc()
        # left
        if x != 0:
          if current_board[y][x - 1] == WoodContents.lumberyard:
            lumber_count.inc()
        # right
        if x < width - 1:
          if current_board[y][x + 1] == WoodContents.lumberyard:
            lumber_count.inc()
        # bottom left
        if x != 0 and y < height - 1:
          if current_board[y + 1][x - 1] == WoodContents.lumberyard:
            lumber_count.inc()
        # bottom
        if y < height - 1:
          if current_board[y + 1][x] == WoodContents.lumberyard:
            lumber_count.inc()
        # bottom right
        if x < width - 1 and y < height - 1:
          if current_board[y + 1][x + 1] == WoodContents.lumberyard:
            lumber_count.inc()
      case cell:
        of WoodContents.open:
          # becomes wooded if there are some trees nearby
          if wooded_count >= 3:
            result[y][x] = WoodContents.trees
          else:
            result[y][x] = cell
        of WoodContents.trees:
          if lumber_count >= 3:
            result[y][x] = WoodContents.lumberyard
          else:
            result[y][x] = cell
        of WoodContents.lumberyard:
          # remains a lumberyard there is both a wooded area and another lumberyard nearby
          if wooded_count >= 1 and lumber_count >= 1:
            result[y][x] = cell
          # otherwise is reverts to an open area
          else:
            result[y][x] = WoodContents.open

# resource value is number of wooded areas multipied by number of lumberyards
proc count_resources(current_board: Landscape): int =
  var
    wooded_acres, lumberyards: int
  for line in current_board:
    for cell in line:
      case cell:
        of WoodContents.lumberyard:
          lumberyards.inc()
        of WoodContents.trees:
          wooded_acres.inc()
        else:
          continue
  return wooded_acres * lumberyards

var
  board: Landscape
  input_file = "input.txt"
  time = 1000000000

# load initial state
board = build_tracks(input_file)

echo "Gen 0"
board.print_track()

for sec in 1 .. time:
  # calculate the landscape after one 'tick' of time
  board = board.tick()
  echo "Gen ", sec
  board.print_track()

echo "Part 1: ", board.count_resources()
