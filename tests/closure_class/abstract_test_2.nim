import oop_utils/closure_class
import strformat

class(Abstract):
  proc id*(): string

class(Sub of Abstract):
  ctor(newSub)
  proc id*(): string {.override.} = "impl"

class(SubSub of Sub):
  # Macro must detect that SubSub is not abstract, because
  # its parent already implements all abstract methods.
  ctor(newSubSub)
  base()

block:
  let a: Abstract = newSubSub()
  doAssert a.id() == "impl"
