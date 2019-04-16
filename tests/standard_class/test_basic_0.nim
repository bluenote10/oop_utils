import oop_utils/standard_class
import strformat

proc generateId(): int =
  42

class(Person):
  ctor proc(prename: string, surname: string) =
    echo "pre-construction"
    let fullName = prename & " " & surname
    self:
      x = 1
      id = generateId()
      prename
      surname`*`
      fullname`*`
    echo "post-construction"


block:
  let p = Person.init("John", "Doe")
  doAssert p.id == 42
  doAssert p.prename == "John"
  doAssert p.surname == "Doe"

