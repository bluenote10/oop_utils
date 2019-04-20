import oop_utils/closure_class
import strformat

class(Person):
  ctor(newPerson) proc(name: string, initAge: int)

  let name = name
  var age = initAge

  proc getAge*(): int = age

  proc greet*() =
    echo &"Hi, I'm {name} and I'm {self.getAge()} years old."

  proc getOlder*() =
    age += 1

block:
  let p: Person = newPerson("John", 42)
  p.greet()
  p.getOlder()
  p.greet()
  p.getOlder()
  p.greet()
  # Hi, I'm John and I'm 42 years old.
  # Hi, I'm John and I'm 43 years old.
  # Hi, I'm John and I'm 44 years old.



class(HackedPerson of Person):
  ctor(newHackedPerson) proc(name: string, initAge: int)

  base(name & " Cheating", initAge)

  var age = initAge.float

  proc getAge*(): int {.override.} = age.int

  proc getOlder*() {.override.} =
    age += 0.5

block:
  let p: Person = newHackedPerson("John", 42)
  p.greet()
  p.getOlder()
  p.greet()
  p.getOlder()
  p.greet()
  # Hi, I'm John Cheating and I'm 42 years old.
  # Hi, I'm John Cheating and I'm 42 years old.
  # Hi, I'm John Cheating and I'm 43 years old.
