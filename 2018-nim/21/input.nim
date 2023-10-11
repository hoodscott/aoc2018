import sets

var
  prev = int.low
  prevValues = initHashSet[int]()
  hasHalted = false

var
  # 15_883_666 # min-input
  r0 = 0
  #00  seti 123 0 1
  r1 = 123
  r3 = 0
  r5 = 0

block start:
  while true:
    #01  bani 1 456 1
    r1 = r1 and 456
    #02  eqri 1 72 1
    #03  addr 1 4 4
    #04  seti 0 0 4
    if (r1 == 72):
      #05  seti 0 7 1
      r1 = 0

      # recalc r1:
      while true:
        #06  bori 1 65536 5
        r5 = r1 or 65536
        #07  seti 8595037 6 1
        r1 = 8595037

        # inner
        while (true):
          #08  bani 5 255 3
          r3 = r5 and 255
          #09  addr 1 3 1
          r1 = r1 + r3
          #10  bani 1 16777215 1
          r1 = r1 and 16777215
          #11  muli 1 65899 1
          r1 = r1 * 65899
          #12  bani 1 16777215 1
          r1 = r1 and 16777215
          #13  gtir 256 5 3
          #14  addr 3 4 4
          #15  addi 4 1
          #16  seti 27 4 4
          if (256 > r5):
            #28  eqrr 1 0 3
            #29  addr 3 4 4
            #30  seti 5 9 4
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
            #17  seti 0 2 3
            #.. instructions here just divide by 256 very slowly
            #26  setr 3 8 5
            r5 = r5 div 256
            #27  seti 7 5 4
            # goto inner
