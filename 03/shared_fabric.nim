import strutils
import sequtils

type
  Claim = ref object of RootObj
    id*: string
    start_X*: int
    start_Y*: int
    width*: int
    height*: int

# get claims into a sequence
# also calculates the maximum x and y values required later
proc import_claims (f_name: string): (seq[Claim], int, int) = 
  var
    read_claims: seq[Claim]
    max_x = 0
    max_y = 0
  for line in lines f_name:
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
    read_claims.add(input_claim)
  return (read_claims, max_x, max_y)

proc create_fabric_map (width: int, height: int, added_claims: seq[Claim]): seq[seq[int]] =
  # create map based on maximum values required
  result = newSeqWith(height + 1, newSeq[int](width + 1))
  for claim in added_claims:
    for x in claim.start_X .. claim.start_X + claim.width - 1:
      for y in claim.start_Y .. claim.start_Y + claim.height - 1:
        result[y][x].inc()

proc double_requested_total (fabric: seq[seq[int]]): int =
  for row in fabric:
    for inch in row:
      if inch > 1:
        result.inc()

proc no_overlap (fabric: seq[seq[int]], check_claims: seq[Claim]): string =
  for claim in check_claims:
    block check_claim:
      for x in claim.start_X .. claim.start_X + claim.width - 1:
        for y in claim.start_Y .. claim.start_Y + claim.height - 1:
          if fabric[y][x] != 1:
            break check_claim
      return claim.id

var
  claims: seq[Claim]
  max_height: int
  max_width: int
  fabric_map: seq[seq[int]]

# read claims from file
(claims, max_width, max_height) = import_claims("input.txt")

# add claims to the map
fabric_map = create_fabric_map(max_width, max_height, claims)

# check how many inches are double requested
echo "Part 1 Answer: ", fabric_map.double_requested_total()

# go back through the claims to find one that has no overlap
echo "Part 2 Answer: ", fabric_map.no_overlap(claims)