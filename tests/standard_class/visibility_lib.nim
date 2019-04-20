import oop_utils/standard_class

class(Test):
  ctor(newTest):
    self:
      a`*` = 1
      b`+` = 2
      c    = 3

block:
  let t = newTest()
  doAssert t.a == 1
  doAssert t.b == 2
  doAssert t.c == 3