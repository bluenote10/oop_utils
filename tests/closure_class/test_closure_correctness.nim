import oop_utils/closure_class

class(Test):
  ctor(newTest) proc(s: openarray[int])

  var s = @s

  proc replace*(newS: openarray[int]) =
    s = @newS

  proc len*(): int = s.len
  proc get*(i: int): int = s[i]
  proc getAll*(): seq[int] = s

block:
  let t = newTest([1, 2, 3])

  echo t.getAll()
  doAssert t.len() == 3
  doAssert t.get(0) == 1
  doAssert t.get(1) == 2
  doAssert t.get(2) == 3

  t.replace([4, 5])

  echo t.getAll()
  doAssert t.len() == 2
  doAssert t.get(0) == 4
  doAssert t.get(1) == 5
