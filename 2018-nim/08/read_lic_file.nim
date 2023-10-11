import strutils, sequtils

# part 1 - sum_meta
# given list of integers representing nodes, where first int indicates the
# number of child nodes, second int the number of metadata ints, next series of
# ints are the child node numbers, then last few ints are the actual metadata ints.
# return the sum of all metadata ints

# part 2 - calc_value
# given the above input, calculate the node's value as either its sum of metadata
# if it is a leaf node or use its metadata to index its child nodes values and sum
# those instead (if index does not exist then default to 0)

type
  Node = ref object of RootObj
    num_children*: int
    num_metadata*: int
    children*: seq[Node]
    metadata*: seq[int]

# read file into sequence of integers
proc read_file_into_seq(f_name: string): seq[int] =
  for i in read_file(f_name).split():
    result.add(i.parseInt())

# create nodes recursively from sequence
proc create_node(sequence: var seq[int]): Node =
  # shift first two entries off as the number of children and metadata
  result = Node(num_children: sequence[0], num_metadata: sequence[1])
  sequence.delete(0)
  sequence.delete(0)
  # create the required number of children
  for child in 1 .. result.num_children:
    # add a child node
    result.children.add(create_node(sequence))
  # create the required number of metadata entries
  for meta in 1 .. result.num_metadata:
    # shift the first entry off as metadata
    result.metadata.add(sequence[0])
    sequence.delete(0)

# sum the total value of metadata entries
proc sum_meta(node: Node): int =
  result = node.metadata.foldl(a + b)
  for child in node.children:
    result += child.sum_meta()

# sum the total value of metadata entries
proc calc_value(node: Node): int =
  # if node has no children, node value is just sum of metadata
  if node.num_children == 0:
    return node.metadata.foldl(a + b)
  # otherwise go through metadata entries
  for meta in node.metadata:
    # only add valid child node values
    if meta > 0 and meta <= node.num_children:
      result += calc_value(node.children[meta - 1])

var
  root: Node
  input_sequence: seq[int]

input_sequence = read_file_into_seq("input.txt")

root = input_sequence.create_node()

echo "Part 1: ", root.sum_meta()

echo "Part 2: ", root.calc_value()
