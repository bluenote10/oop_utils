import oop_utils/closure_class

class(Base):
  ctor(newBase) proc(xInit: int = 10)

  var x = xInit

  proc inc() =
    x += 1

  proc getState*(): int =
    inc()
    x

block:
  let b = newBase()
  doAssert b.getState() == 11
  doAssert b.getState() == 12