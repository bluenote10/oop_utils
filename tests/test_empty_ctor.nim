import closure_methods

class(A):
  ctor(newA)
  proc x*(): int = 42

class(B):
  ctor(newB)
  proc x*(): int = 42

block:
  doAssert newA().x() == 42
  doAssert newB().x() == 42
