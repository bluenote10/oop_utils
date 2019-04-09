import closure_methods
import strformat

class(Abstract):
  proc id*(): string

class(Impl of Abstract):
  ctor(newImpl)
  proc id*(): string {.override.} = "impl"

block:
  let a: Abstract = newImpl()
  doAssert a.id() == "impl"


class(AbstractWithInit):
  ctor proc(x: int)
  var x = x
  proc abstract*(): int
  proc compute*(): int = self.abstract() + x

class(StillAbstract of AbstractWithInit):
  base(10)

class(A of AbstractWithInit):
  ctor proc(x: int)
  base(x)
  proc abstract*(): int {.override.} = 5

class(B of StillAbstract):
  base() # TODO: get rid of empty base call
  proc abstract*(): int {.override.} = 6

block:
  let a = A.init(20)
  let b = B.init()
  doAssert a.compute() == 25
  doAssert b.compute() == 16