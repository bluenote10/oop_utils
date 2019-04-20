import oop_utils/closure_class

class(Base):
  ctor(newBase) proc(baseInit: string)

  var state = baseInit

  proc getState*(): string = state


class(Child of Base):
  ctor(newChild) proc()

  base("base")

  var state = "child"

  proc getState*(): string {.override.} = state


block:
  let b = newBase("base")
  assert b.getState() == "base"

  let c = newChild()
  assert c.getState() == "child"