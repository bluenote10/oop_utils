import closure_methods

class(Base):
  constructor:
    proc newBase*(baseInit: string)

  var state = baseInit

  proc getState*(): string = state


classOf(Child, Base):
  constructor:
    proc newChild*()

  base("base")

  var state = "child"

  proc getState*(): string = state


block:
  let b = newBase("base")
  assert b.getState() == "base"

  let c = newChild()
  assert c.getState() == "child"