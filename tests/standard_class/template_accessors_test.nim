import oop_utils/standard_class
import strformat

class(Test):
  ctor proc() =
    self:
      a = 0

  template getA*(): int =
    self.a
  template setA*(x: int) =
    self.a = x

block:
  let t = Test.init()
  doAssert t.getA == 0
  t.setA(1)
  doAssert t.getA == 1