import closure_methods

class(Base):
  # upcoming ctor syntax to make naming ctor optional
  # constructor(newBase) =
  #   proc(xInit: int = 10)

  constructor:
    proc newBase*(xInit: int = 10)

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