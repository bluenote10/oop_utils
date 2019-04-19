import oop_utils/standard_class
import strformat

class(Abstract):
  method id*(): string {.base.}

class(ImplA of Abstract):
  method id*(): string = "implA"

class(ImplB of Abstract):
  ctor(newImplB)
  method id*(): string = "implB"

class(ImplC of Abstract):
  ctor(newImplC) proc(x: int) =
    echo "pre"
    self:
      x
    echo "post"

  method id*(): string = "implC"

block:
  let a: Abstract = ImplA.init()
  let b: Abstract = newImplB()
  let c: Abstract = newImplC(1)
  doAssert a.id() == "implA"
  doAssert b.id() == "implB"
  doAssert c.id() == "implC"