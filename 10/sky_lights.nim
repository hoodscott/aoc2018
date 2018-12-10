import strutils, sequtils, re

type
  Position = ref object
    x_pos: int
    y_pos: int
  Light = ref object
    position: Position
    x_velocity: int
    y_velocity: int
  Bounds = ref object
    min_x: int
    max_x: int
    min_y: int
    max_y: int

# read file to get initial positions and velocities
proc get_initial_points(f_name: string): seq[Light] =
  var
    bounds = Bounds(min_x: high(int), max_x: low(int), min_y: high(int), max_y: low(int))
  for line in lines f_name:
    var
      line_arr = line.findAll(re"-?\d+").map(parseInt)
    result.add(Light(position: Position(x_pos: line_arr[0], y_pos: line_arr[1]),
                      x_velocity: line_arr[2], y_velocity: line_arr[3]))

# calculate the smallest box that fits all lights
proc calc_bounds(l: seq[Light]): Bounds =
  result = Bounds(min_x: l.foldl(min(a,b.position.x_pos), high(int)),
                  max_x: l.foldl(max(a,b.position.x_pos), low(int)),
                  min_y: l.foldl(min(a,b.position.y_pos), high(int)),
                  max_y: l.foldl(max(a,b.position.y_pos), low(int)))

# show the lights in a grid
proc display(lights: seq[Light], bounds: Bounds): void =
  for y in bounds.min_y .. bounds.max_y:
    var
      line: string
    for x in bounds.min_x .. bounds.max_x:
      var
        pixel = '.'
      for l in lights:
        if l.position.x_pos == x and l.position.y_pos == y:
          pixel = '#'
      line.add(pixel)
    echo line

# using the velocity of the light, calculate its new position
proc move(l: Light, time: int): Light =
  for second in 1 .. time:
    l.position.x_pos += l.x_velocity
    l.position.y_pos += l.y_velocity
  return l

# move each light one second forward in time
proc tick(lights: var seq[Light]): void =
  for index, l in lights:
    lights[index] = l.move(1)

var
  lights: seq[Light]
  boundary: Bounds
  seconds: int

# read file to get initial positions and velocities
lights = get_initial_points("input.txt")

# loop until we manually break out
while true:
  # increment the second counter
  seconds.inc()
  # move the lights according to their velocities
  lights.tick()
  # calculate the smallest box that fits all lights
  boundary = lights.calc_bounds()
  echo "After ", seconds, " second(s), vertical boundary is: ", boundary.max_y - boundary.min_y
  # once the boundary is 10 high (height of letters),
  if boundary.max_y - boundary.min_y < 10:
    # we can display the letter and stop the loop as the lights will begin to diverge
    lights.display(boundary)
    break
