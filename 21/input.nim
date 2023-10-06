import sets

var
  prev = int.low
  prevValues = initHashSet[int]()
  hasHalted = false

var
  # r0 = 15_883_666 # min-input
  r0 = 0
  r1 = 123
  r3 = 0
  r5 = 0

block start:
  while true:
    r1 = r1 and 456
    if (r1 == 72):
      r1 = 0

      # recalc r1:
      while true:
        r5 = r1 or 65536

        r1 = 8595037

        while (true):
          r3 = r5 and 255
          r1 = r1 + r3
          r1 = r1 and 16777215
          r1 = r1 * 65899
          r1 = r1 and 16777215
          if (256 > r5):
            if (r1 == r0):
              break start

            # output r1 here to see what r0 should be to halt
            if not hasHalted:
              echo "Part 1: ", r1
              hasHalted = true

            # track values so we find first instance of repeat and return previous
            if r1 in prevValues:
              echo "Part 2: ", prev
              break start
            prev = r1
            prevValues.incl(prev)

            # else:
            #   goto recalc r1
            break
          else:
            r5 = r5 div 256

  # else
  #   goto start
