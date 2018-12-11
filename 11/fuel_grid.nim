import sequtils

type
  Point = ref object
    x_pos: int
    y_pos: int
    power: int
    width: int

# calculate power level of one cell according to rules
proc calc_power_level(x: int, y: int, serial: int): int =
  var
    rack_id = x + 10
  result = (rack_id * y + serial) * rack_id /% 100 mod 10 - 5

# calculate the power levels for each cell
proc power_levels(grid: var seq[seq[int]], width: int, serial: int): seq[seq[int]] =
  for x in 1 .. width:
    for y in 1 .. width:
      grid[y - 1][x - 1] = calc_power_level(x, y, serial)
  return grid

# create a grid of 3x3 cell sums
proc calc_sums(max_grid: var seq[seq[int]], grid: seq[seq[int]], width: int, sum_size: int): seq[seq[int]] =
  for x in 0 .. width - 1:
    for y in 0 .. width - 1:
      for x1 in x .. x + sum_size - 1:
        for y1 in y .. y + sum_size - 1:
          max_grid[y][x] += grid[y1][x1]
  return max_grid

var
  input = 5719
  width = 300
  grid = newSeqWith(width, newSeq[int](width))
  max_coord: Point

# create grid with power levels
grid = grid.power_levels(width, input)

max_coord = Point(x_pos: 0, y_pos: 0, power: grid[0][0], width: 1)

# create grid with sums
for sum_size in 1 .. 300: 
  var
    grid_width = width - sum_size + 1
    max_grid = newSeqWith(grid_width, newSeq[int](grid_width))
  max_grid = max_grid.calc_sums(grid, grid_width, sum_size)

  # find max sum on each row
  for y, row in max_grid:
    for x, cell in row:
      if cell > max_coord.power:
        max_coord.power = cell
        max_coord.x_pos = x
        max_coord.y_pos = y
        max_coord.width = sum_size

  if sum_size == 3:
    echo "Part 1: ", max_coord.x_pos + 1, ",", max_coord.y_pos + 1, " (", max_coord.power, ")"

  echo "Running Max at ", sum_size, "x", sum_size
  echo max_coord.x_pos + 1, ",", max_coord.y_pos + 1, ",", 
       max_coord.width, " (", max_coord.power, ")"

echo "Part 2: ", max_coord.x_pos + 1, ",", max_coord.y_pos + 1, ",", 
      max_coord.width, " (", max_coord.power, ")"
