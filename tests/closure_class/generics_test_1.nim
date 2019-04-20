import oop_utils/closure_class

class(Base[T]):
  ctor(newBase) proc(xInit: T)

  var x = xInit

  proc getT*(): T = x


block:
  let x = newBase(42)
  doAssert x.getT() == 42
