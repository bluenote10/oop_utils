import oop_utils/standard_class
import strformat

class(Abstract):
  method id*(): string

class(Impl of Abstract):
  ctor(newImpl) proc() = discard  # TODO support empty ctor procs in case of zero fields...
  method id*(): string = "impl"

block:
  let a: Abstract = newImpl()
  doAssert a.id() == "impl"