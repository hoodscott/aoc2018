import strutils
import sequtils

type
  Claim = ref object of RootObj
    id*: string
    start_X*: int
    start_Y*: int
    width*: int
    height*: int

var
  claims: seq[Claim]
  max_x = 0
  max_y = 0
  double_requested = 0

# get claims into a sequence
for line in lines "input.txt":
  var
    claim_array: seq[string]
    input_claim: Claim
  claim_array = line.replace(" ").split({'@', ',', ':', 'x'})
  input_claim = Claim(id: claim_array[0],
                      start_X: claim_array[1].parseInt(), start_y: claim_array[2].parseInt(),
                      width: claim_array[3].parseInt(), height: claim_array[4].parseInt())
  # find the max x and y values so we can create the map
  if input_claim.start_X + input_claim.width > max_x:
    max_x = input_claim.start_X + input_claim.width
  if input_claim.start_Y + input_claim.height > max_y:
    max_y = input_claim.start_Y + input_claim.height
  claims.add(input_claim)

# create map based on maximum values required
var fabric_map = newSeqWith(max_y + 1, newSeq[int](max_x + 1))

# add claims to the map
for claim in claims:
  for x in claim.start_X .. claim.start_X + claim.width - 1:
    for y in claim.start_Y .. claim.start_Y + claim.height - 1:
      fabric_map[y][x] += 1

# check how many inches are double requested
for row in fabric_map:
  for inch in row:
    if inch > 1:
      double_requested += 1

echo double_requested