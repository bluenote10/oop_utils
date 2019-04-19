import oop_utils/standard_class
import strformat


class(A):
  ctor proc(a = "aaa") =
    self:
      a

class(B of A):
  ctor proc() =
    discard

class(C of B):
  ctor proc(c = "ccc") =
    self:
      c

block:
  let c = C.init()
  echo c.a
  echo c.c
  doAssert c.a == "aaa"
  doAssert c.c == "ccc"