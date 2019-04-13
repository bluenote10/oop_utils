import oop_utils/standard_class
import strformat

{.warning[Spacing]: off.}

class(Person):
  ctor proc(name: string) =
    self.name* is string = name

  method sayHello*(): string {.base.} =
    &"Hello, my name is {self.name}."

class(Student of Person):
  ctor(newDog) proc(name: string, studentNumber: int) =
    base(name)
    self.studentNumber is int = studentNumber

  method sayHello*(): string =
    &"Hello, my name is {self.name}, and my student number is {self.studentNumber}."

block:
  let persons = [
    Person.init("John"),
    Student.init("Mike", 42),
  ]
  doAssert persons[0].sayHello == "Hello, my name is John."
  doAssert persons[1].sayHello == "Hello, my name is Mike, and my student number is 42."
