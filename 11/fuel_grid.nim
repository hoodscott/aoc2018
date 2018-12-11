import sequtils

type
  Point = ref object
    x_pos: int
    y_pos: int
    power: int

# calculate power level of one cell according to rules
proc calc_power_level(x: int, y: int, serial: int): int =
  var
    rack_id = x + 10
  result = (rack_id * y + serial) * rack_id /% 100 mod 10 - 5

# calculate the power levels for each cell
proc power_levels(grid: var seq[seq[int]], size: int, serial: int): seq[seq[int]] =
  for x in 1 .. size:
    for y in 1 .. size:
      grid[y - 1][x - 1] = calc_power_level(x, y, serial)
  return grid

# create a grid of 3x3 cell sums
proc calc_sums(max_grid: var seq[seq[int]], grid: seq[seq[int]], size: int): seq[seq[int]] =
  for x in 0 .. size - 1:
    for y in 0 .. size - 1:
      max_grid[y][x] = grid[y][x] + grid[y][x + 1] + grid[y][x + 2] +
                               grid[y + 1][x] + grid[y + 1][x + 1] + grid[y + 1][x + 2] +
                               grid[y + 2][x] + grid[y + 2][x + 1] + grid[y + 2][x + 2]
  return max_grid

var
  input = 5719
  size = 300
  grid = newSeqWith(size, newSeq[int](size))
  max_grid = newSeqWith(size - 2, newSeq[int](size - 2))
  max_coord: Point

# create grid with power levels
grid = grid.power_levels(size, input)

# create grid with sums
max_grid = max_grid.calc_sums(grid, size - 2)

max_coord = Point(x_pos: 0, y_pos: 0, power: max_grid[0][0])

# find max sum on each row
for y, row in max_grid:
  for x, cell in row:
    if cell > max_coord.power:
      max_coord.power = cell
      max_coord.x_pos = x
      max_coord.y_pos = y

echo "Part 1: ", max_coord.x_pos + 1, ",", max_coord.y_pos + 1, " (", max_coord.power, ")"
