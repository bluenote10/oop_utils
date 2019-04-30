import oop_utils/standard_class

class(Base[T]):
  ctor(newBase) proc(xInit: int) =
    self:
      x: T = xInit

  proc getT*(): T = self.x


block:
  let x = newBase[int](42)
  doAssert x.getT() == 42
