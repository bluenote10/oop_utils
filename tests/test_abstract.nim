import closure_methods
import strformat

class(Abstract):
  proc id*(): string

classOf(Impl, Abstract):
  ctor(newImpl)
  proc id*(): string = "impl"

block:
  let a: Abstract = newImpl()
  doAssert a.id() == "impl"
