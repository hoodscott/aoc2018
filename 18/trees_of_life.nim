import sequtils

# part 1 - for sec in 1 .. 10:
# given 2d map of forest where a cell is either open ground, trees, or lumberyard
# simulate time where every minute each cell can change:  open areas become trees
# if surrounded by three or more adjacent trees, trees become lumberyard if three
# adjacent cells are lumberyards, and lumberyards only remain lumberyards if
# adjacent to another lumberyad and trees (otherwise they go back to open ground).
# all changes happen simulataneously.  find the resource value (number of tree
# cells * number of lumberyards) after 10 mins

# part 2 - for sec in 1 .. 1_000_000_000:
# same as above but simulate for 1 billion minutes
# (can see the state cycles so can leave early)

type
  WoodContents {.pure.} = enum
    open, trees, lumberyard
  Landscape = seq[seq[WoodContents]]

proc build_landscape(f_name: string): LandScape =
  var
    height: int
    width: int
    y: int

  # get the dimensions
  for line in lines f_name:
    width = line.len
    height.inc()

  result = newSeqWith(height, newSeq[WoodContents](width))

  # go back through the file to place the landscape features
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

proc print_landscape(current_landscape: Landscape): void =
  for y, line in current_landscape:
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

proc tick(current_landscape: Landscape): Landscape =
  var
    height = current_landscape.len()
    width = current_landscape[0].len()

  # copy the current board and reset all acres
  result = newSeqWith(height, newSeq[WoodContents](width))

  for y, line in current_landscape:
    for x, cell in line:
      var
        wooded_count: int
        lumber_count: int
      if cell != WoodContents.trees:
        # if the area not wooded, count the surrounding wooded acres
        # top left
        if x != 0 and y != 0:
          if current_landscape[y - 1][x - 1] == WoodContents.trees:
            wooded_count.inc()
        # top
        if y != 0:
          if current_landscape[y - 1][x] == WoodContents.trees:
            wooded_count.inc()
        # top right
        if x < width - 1 and y != 0:
          if current_landscape[y - 1][x + 1] == WoodContents.trees:
            wooded_count.inc()
        # left
        if x != 0:
          if current_landscape[y][x - 1] == WoodContents.trees:
            wooded_count.inc()
        # right
        if x < width - 1:
          if current_landscape[y][x + 1] == WoodContents.trees:
            wooded_count.inc()
        # bottom left
        if x != 0 and y < height - 1:
          if current_landscape[y + 1][x - 1] == WoodContents.trees:
            wooded_count.inc()
        # bottom
        if y < height - 1:
          if current_landscape[y + 1][x] == WoodContents.trees:
            wooded_count.inc()
        # bottom right
        if x < width - 1 and y < height - 1:
          if current_landscape[y + 1][x + 1] == WoodContents.trees:
            wooded_count.inc()
      if cell != WoodContents.open:
        # if the area is not open, count the surrounding lumberyards
        # top left
        if x != 0 and y != 0:
          if current_landscape[y - 1][x - 1] == WoodContents.lumberyard:
            lumber_count.inc()
        # top
        if y != 0:
          if current_landscape[y - 1][x] == WoodContents.lumberyard:
            lumber_count.inc()
        # top right
        if x < width - 1 and y != 0:
          if current_landscape[y - 1][x + 1] == WoodContents.lumberyard:
            lumber_count.inc()
        # left
        if x != 0:
          if current_landscape[y][x - 1] == WoodContents.lumberyard:
            lumber_count.inc()
        # right
        if x < width - 1:
          if current_landscape[y][x + 1] == WoodContents.lumberyard:
            lumber_count.inc()
        # bottom left
        if x != 0 and y < height - 1:
          if current_landscape[y + 1][x - 1] == WoodContents.lumberyard:
            lumber_count.inc()
        # bottom
        if y < height - 1:
          if current_landscape[y + 1][x] == WoodContents.lumberyard:
            lumber_count.inc()
        # bottom right
        if x < width - 1 and y < height - 1:
          if current_landscape[y + 1][x + 1] == WoodContents.lumberyard:
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
proc count_resources(current_landscape: Landscape): int =
  var
    wooded_acres, lumberyards: int
  for line in current_landscape:
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
  landscape: Landscape
  input_file = "input.txt"
  debug = false
  prev_resources = 0

# load initial state
landscape = build_landscape(input_file)

if debug:
  echo "Gen 0"
  landscape.print_landscape()

for minute in 1 .. 10:
  # calculate the landscape after one 'tick' of time
  landscape = landscape.tick()
  if debug:
    echo "Gen ", minute
    landscape.print_landscape()

echo "Part 1: ", landscape.count_resources()

# reload initial state
landscape = build_landscape(input_file)

for minute in 1 .. 1_000_000_000:
  # calculate the landscape after one 'tick' of time
  landscape = landscape.tick()
  # sequence repeats every 35 cycles (and 1 billion mod 35 is 20)
  if minute mod 35 == 20:
    let current_resources = landscape.count_resources()
    if prev_resources == current_resources:
      echo "Part 2: ", current_resources
      break
    prev_resources = current_resources

