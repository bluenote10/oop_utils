import oop_utils/closure_class
import strformat

class(Person):
  ctor proc(name: string)
  var name = "internal_" & name
  proc getName*(): string = name

block:
  let p = Person.init("name")
  echo p.getName()
  doAssert p.getName() == "internal_name"
