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


class(AbstractWithInit):
  ctor proc(x: int)
  var x = x
  proc abstract*(): int
  proc compute*(): int = self.abstract() + x

classOf(StillAbstract, AbstractWithInit):
  base(10)

classOf(A, AbstractWithInit):
  ctor proc(x: int)
  base(x)
  proc abstract*(): int = 5

classOf(B, StillAbstract):
  base() # TODO: get rid of empty base call
  proc abstract*(): int = 6

block:
  let a = A.init(20)
  let b = B.init()
  doAssert a.compute() == 25
  doAssert b.compute() == 16