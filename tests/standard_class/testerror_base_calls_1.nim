#[
Class needs to have an explicit base call, because the constructor of 'A' requires parameters.
]#
import oop_utils/standard_class
import strformat


class(A):
  ctor proc(required: int) =
    self:
      a = "a"

class(B of A):
  ctor proc() =
    self:
      b = "b"
