import strutils

type
  Coords = ref object of RootObj
    x_pos*: int
    y_pos*: int
    count*: int

proc read_file(f_name: string):  seq[Coords] =
  var
    result = newSeq[Coords]()
  for line in lines f_name:
    result.add(Coords(x_pos: line.split(", ")[0].parseInt(),
                      y_pos: line.split(", ")[1].parseInt(),
                      count: 0))
  return result

var
  coords: seq[Coords]
  max_x = -1
  max_y = -1

coords = read_file("test_input.txt")

for x in coords:
  if x.x_pos > max_x:
    max_x = x.x_pos
  if x.y_pos > max_y:
    max_y = x.y_pos

echo "maxx", max_x
echo "maxy", max_y
