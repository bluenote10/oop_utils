import oop_utils/standard_class
import strformat


class(A):
  ctor proc(a = "aaa") =
    self:
      a

class(B of A):
  ctor proc(b = "bbb") =
    self:
      b

class(C of B):
  ctor proc(c = "ccc") =
    self:
      c

block:
  let c = C.init()
  doAssert c.a == "aaa"
  doAssert c.b == "bbb"
  doAssert c.c == "ccc"
