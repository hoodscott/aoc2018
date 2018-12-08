import strutils, sequtils

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
    result.children.add(sequence.create_node)
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
  # if node has no children
  if node.num_children == 0:
    # node value is just sum of metadata
    return node.metadata.foldl(a + b)
  # otherwise go through metadata entries
  for meta in node.metadata:
    # make sure we have a child node at this index
    if meta > 0 and meta <= node.num_children:
      result += node.children[meta - 1].calc_value()

var
  root: Node
  input_sequence: seq[int]

input_sequence = read_file_into_seq("input.txt")

root = input_sequence.create_node()

echo "Part 1: ", root.sum_meta()

echo "Part 2: ", root.calc_value()
