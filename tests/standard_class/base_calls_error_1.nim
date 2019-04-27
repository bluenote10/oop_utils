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

# Note: If we use generic instantiations of the patch function,
# we need to explicitly construct the class in order to trigger
# the compile time error. This is a significant drawback, so for
# now we try to avoid making patch generic...
block:
  let b = B.init()