import strutils, sequtils

type
  Coords = ref object of RootObj
    x_pos*: int
    y_pos*: int
  Nearest = ref object of RootObj
    id*: int
    distance*: int

# read file in as a sequence of coordinates
proc read_file(f_name: string):  seq[Coords] =
  for line in lines f_name:
    result.add(Coords(x_pos: line.split(", ")[0].parseInt(),
                      y_pos: line.split(", ")[1].parseInt()))

# build a grid big enough to fit our co-ords
# with points to be filled in later on another pass
proc create_grid(width: int, height: int): seq[seq[Nearest]] =
  for y in 0 .. height:
    var
      row: seq[Nearest]
    for x in 0 .. width:
      row.add(Nearest(id: -1, distance: high(int)))
    result.add(row)

proc calc_manhattan_distance(x1, y1, x2, y2: int): int =
  return (x1 - x2).abs() + (y1 - y2).abs()

# fill in the grid with the points closest to each coord
proc add_coordinates(base_grid: seq[seq[Nearest]], coordinates: seq[Coords]): seq[seq[Nearest]] =
  for index, coord in coordinates:
    for y, row in base_grid:
      for x, point in row:
        var
          manhattan_dist: int
        manhattan_dist = calc_manhattan_distance(x, y, coord.x_pos, coord.y_pos)
        # if the distances are the same, this perfectly between two coordinates
        if manhattan_dist == point.distance:
          point.id = -1
        elif manhattan_dist < point.distance:
          point.id = index
          point.distance = manhattan_dist
  return base_grid

# go around the edge of the grid to remove the infinite areas
proc find_infinite_areas(full_grid: seq[seq[Nearest]]): set[int16] =
  var
    width: int
    height: int
  width = full_grid[0].len()
  height = full_grid.len()
  for x in 0 .. width - 1:
    # top row
    if not result.contains(full_grid[0][x].id.int16):
      result.incl(full_grid[0][x].id.int16)
    # bottom row
    if not result.contains(full_grid[height - 1][x].id.int16):
      result.incl(full_grid[height - 1][x].id.int16)
  # top and bottom rows already checked so can start and end sooner
  for y in 1 .. height - 2:
    # first column
    if not result.contains(full_grid[y][0].id.int16):
      result.incl(full_grid[y][0].id.int16)
    # last column
    if not result.contains(full_grid[y][width - 1].id.int16):
      result.incl(full_grid[y][width - 1].id.int16)

# find the coord with the largest non-infinite area surrounding it
proc find_largest_area(full_grid: seq[seq[Nearest]], coordinates: seq[Coords], excluded: set[int16]): int =
  var
    largest_index: int
  result = -1
  for index, coord in coordinates:
    # skip the infinite area coords
    if not excluded.contains(index.int16):
      var
        count: int
      # sum area
      for row in full_grid:
        for point in row:
          if point.id == index:
            count.inc()
      # check if it is bigger than the current max
      if count > result:
        result = count
        largest_index = index

var
  coords: seq[Coords]
  grid: seq[seq[Nearest]]
  infinites: set[int16]
  largest_area: int
  max_x = -1
  max_y = -1

coords = read_file("input.txt")

for x in coords:
  if x.x_pos > max_x:
    max_x = x.x_pos
  if x.y_pos > max_y:
    max_y = x.y_pos

grid = create_grid(max_x, max_y)

grid = grid.add_coordinates(coords)

infinites = grid.find_infinite_areas()

largest_area = grid.find_largest_area(coords, infinites)

echo "Part 1:", largest_area