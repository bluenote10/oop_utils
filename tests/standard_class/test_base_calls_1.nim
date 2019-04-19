import oop_utils/standard_class
import strformat


class(A):
  ctor proc() =
    self:
      a = "a"

class(B of A):
  ctor proc() =
    self:
      b = "b"

class(C of B):
  ctor proc() =
    self:
      c = "c"

block:
  let c = C.init()
  doAssert c.a == "a"
  doAssert c.b == "b"
  doAssert c.c == "c"