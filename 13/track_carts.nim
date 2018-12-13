import sequtils, strutils

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
          this_cart = Cart(dir: CardinalDirections.west, next_turn: Decisions.low())
        elif symbol == '>':
            this_cart = Cart(dir: CardinalDirections.east, next_turn: Decisions.low())
      elif symbol == '^' or symbol == 'v':
        track_symbol = '|'
        if symbol == '^':
          this_cart = Cart(dir: CardinalDirections.north, next_turn: Decisions.low())
        elif symbol == 'v':
            this_cart = Cart(dir: CardinalDirections.south, next_turn: Decisions.low())
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
  result[1] = newSeqWith(current_board.len(), newSeq[Track](current_board[0].len()))

  for y, line in current_board:
    for x, cell in line:
      result[1][y][x] = Track(path: cell.path)
  
  for y, line in current_board:
    for x, cell in line:
      if cell.cart != nil:
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

var
  board: seq[seq[Track]]
  collided: bool
  collision: (bool, int, int)

board = build_tracks("input.txt")

board.print_track()

while not collision[0]:
  (collision, board) = board.tick()
  board.print_track()

echo "Part 1: ", collision[1], ", ", collision[2]
