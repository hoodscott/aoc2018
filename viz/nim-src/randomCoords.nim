import std/random #, std/strutils

randomize()

const
  min = -50
  max = 50
  dim = 3
  count = 100

var str = "["
for i in 0..count:
  var strInner = ""
  for d in 0..<dim:
    strInner.add($rand(min..max) & ", ")
  str.add("(" & strInner[0..^3] & "), ")
str = str[0..^3] & "]"

echo str
