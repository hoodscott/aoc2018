import sequtils

# part 1 - while not collision[0]
# given a map of overlapping minecart tracks with minecarts placed on them
# with an indicated direction, simulate carts moving until there is a crash.
# carts will decide when to turn by going left, straight, then right (looping)
# for each intersection they come across.  return the coord of the first crash

# part 2 - while board.count_carts() > 1
# same as above but when two carts crash, remove them from the simulation and
# continue until only one cart remains.  return the coord of this cart one tick
# after the last crash occurs

type
  Decisions {.pure.} = enum
    left, straight, right
  CardinalDirections {.pure.} = enum
    north, east, south, west
  Track = ref object
    path: char
    cart: Cart
  Cart = ref object
    dir: CardinalDirections
    next_turn: Decisions
    moved: bool

proc build_tracks(f_name: string): seq[seq[Track]] =
  var
    height: int
    width: int
    y: int

  # get the dimensions
  for line in lines f_name:
    width = line.len
    height.inc()

  result = newSeqWith(height, newSeq[Track](width))

  # go back through the file to get the track and cart positions
  for line in lines f_name:
    for x, symbol in line:
      var
        track_symbol: char
        this_cart: Cart
      if symbol == '<' or symbol == '>':
        track_symbol = '-'
        if symbol == '<':
          this_cart = Cart(dir: CardinalDirections.west,
              next_turn: Decisions.low())
        elif symbol == '>':
          this_cart = Cart(dir: CardinalDirections.east,
              next_turn: Decisions.low())
      elif symbol == '^' or symbol == 'v':
        track_symbol = '|'
        if symbol == '^':
          this_cart = Cart(dir: CardinalDirections.north,
              next_turn: Decisions.low())
        elif symbol == 'v':
          this_cart = Cart(dir: CardinalDirections.south,
              next_turn: Decisions.low())
      else:
        track_symbol = symbol
      result[y][x] = Track(path: track_symbol, cart: this_cart)
    y.inc()

proc print_track(current_track: seq[seq[Track]]): void =
  for y, line in current_track:
    var
      printed_line: string
    for x, cell in line:
      if cell.cart != nil:
        printed_line.add('x')
      else:
        printed_line.add(cell.path)
    echo printed_line

proc tick(current_board: seq[seq[Track]]): ((bool, int, int), seq[seq[Track]]) =
  # copy the current board and remove all carts
  result[1] = current_board

  for y, line in result[1]:
    for x, cell in line:
      result[1][y][x] = Track(path: cell.path, cart: cell.cart)
      if result[1][y][x].cart != nil:
        result[1][y][x].cart.moved = false

  for y, line in result[1]:
    for x, cell in line:
      if cell.cart != nil and not cell.cart.moved:
        cell.cart.moved = true
        # should the cart change direction
        if cell.path == '+':
          case cell.cart.next_turn:
            of Decisions.left:
              cell.cart.dir = CardinalDirections((cell.cart.dir.ord - 1 + 4) mod 4)
            of Decisions.straight:
              cell.cart.dir = cell.cart.dir
            of Decisions.right:
              cell.cart.dir = CardinalDirections((cell.cart.dir.ord + 1) mod 4)
          # set up the decision for the next turn
          cell.cart.next_turn = Decisions((cell.cart.next_turn.ord + 1) mod 3)
        elif (cell.path == '/' and cell.cart.dir == CardinalDirections.east) or
          (cell.path == '\\' and cell.cart.dir == CardinalDirections.west):
          cell.cart.dir = CardinalDirections.north
        elif (cell.path == '/' and cell.cart.dir == CardinalDirections.west) or
          (cell.path == '\\' and cell.cart.dir == CardinalDirections.east):
          cell.cart.dir = CardinalDirections.south
        elif (cell.path == '/' and cell.cart.dir == CardinalDirections.north) or
          (cell.path == '\\' and cell.cart.dir == CardinalDirections.south):
          cell.cart.dir = CardinalDirections.east
        elif (cell.path == '/' and cell.cart.dir == CardinalDirections.south) or
          (cell.path == '\\' and cell.cart.dir == CardinalDirections.north):
          cell.cart.dir = CardinalDirections.west
        # move the cart along its current direction
        case cell.cart.dir:
          of CardinalDirections.north:
            if result[1][y - 1][x].cart != nil:
              return ((true, x, y - 1), result[1])
            result[1][y - 1][x].cart = cell.cart
          of CardinalDirections.east:
            if result[1][y][x + 1].cart != nil:
              return ((true, x + 1, y), result[1])
            result[1][y][x + 1].cart = cell.cart
          of CardinalDirections.south:
            if result[1][y + 1][x].cart != nil:
              return ((true, x, y + 1), result[1])
            result[1][y + 1][x].cart = cell.cart
          of CardinalDirections.west:
            if result[1][y][x - 1].cart != nil:
              return ((true, x - 1, y), result[1])
            result[1][y][x - 1].cart = cell.cart
        result[1][y][x].cart = nil

proc tick_delete(current_board: seq[seq[Track]]): (seq[seq[Track]]) =
  # copy the current board and remove all carts
  result = current_board

  for y, line in result:
    for x, cell in line:
      if result[y][x].cart != nil:
        result[y][x].cart.moved = false

  for y, line in result:
    for x, cell in line:
      if cell.cart != nil and not cell.cart.moved:
        cell.cart.moved = true
        # should the cart change direction
        if cell.path == '+':
          case cell.cart.next_turn:
            of Decisions.left:
              cell.cart.dir = CardinalDirections((cell.cart.dir.ord - 1 + 4) mod 4)
            of Decisions.straight:
              cell.cart.dir = cell.cart.dir
            of Decisions.right:
              cell.cart.dir = CardinalDirections((cell.cart.dir.ord + 1) mod 4)
          # set up the decision for the next turn
          cell.cart.next_turn = Decisions((cell.cart.next_turn.ord + 1) mod 3)
        elif (cell.path == '/' and cell.cart.dir == CardinalDirections.east) or
          (cell.path == '\\' and cell.cart.dir == CardinalDirections.west):
          cell.cart.dir = CardinalDirections.north
        elif (cell.path == '/' and cell.cart.dir == CardinalDirections.west) or
          (cell.path == '\\' and cell.cart.dir == CardinalDirections.east):
          cell.cart.dir = CardinalDirections.south
        elif (cell.path == '/' and cell.cart.dir == CardinalDirections.north) or
          (cell.path == '\\' and cell.cart.dir == CardinalDirections.south):
          cell.cart.dir = CardinalDirections.east
        elif (cell.path == '/' and cell.cart.dir == CardinalDirections.south) or
          (cell.path == '\\' and cell.cart.dir == CardinalDirections.north):
          cell.cart.dir = CardinalDirections.west
        # move the cart along its current direction
        case cell.cart.dir:
          of CardinalDirections.north:
            if result[y - 1][x].cart != nil:
              result[y - 1][x].cart = nil
            else:
              result[y - 1][x].cart = cell.cart
          of CardinalDirections.east:
            if result[y][x + 1].cart != nil:
              result[y][x + 1].cart = nil
            else:
              result[y][x + 1].cart = cell.cart
          of CardinalDirections.south:
            if result[y + 1][x].cart != nil:
              result[y + 1][x].cart = nil
            else:
              result[y + 1][x].cart = cell.cart
          of CardinalDirections.west:
            if result[y][x - 1].cart != nil:
              result[y][x - 1].cart = nil
            else:
              result[y][x - 1].cart = cell.cart
        result[y][x].cart = nil

proc count_carts(current_board: seq[seq[Track]]): int =
  for line in current_board:
    for cell in line:
      if cell.cart != nil:
        result.inc()

proc echo_remaining_cart(current_board: seq[seq[Track]]): void =
  for y, line in current_board:
    for x, cell in line:
      if cell.cart != nil:
        echo "Part 2: ", x, ", ", y

var
  board: seq[seq[Track]]
  collision: (bool, int, int)
  count: int
  input_file = "input.txt"
  debug = false

board = build_tracks(input_file)

if debug:
  print_track(board)

while not collision[0]:
  (collision, board) = board.tick()

echo "Part 1: ", collision[1], ", ", collision[2]

board = build_tracks(input_file)

while board.count_carts() > 1:
  board = board.tick_delete()
  if debug and count mod 1000 == 0:
    echo "Generation: ", count, "; Num. Carts: ", board.count_carts()
  count.inc()

if debug:
  echo "Generation: ", count
echo_remaining_cart(board)
