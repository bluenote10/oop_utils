import closure_methods

class(Base):
  #constructor(newBase) =
  #  proc(xInit: int = 10)

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
  echo b.getState()
  echo b.getState()