import oop_utils

{.push warning[Spacing]: off.}

when false:
  class(Base):
    ctor(newBase) proc(xInit: int = 10) =
      #self.x+: int = xInit
      self.x* is int = xInit
      x is int = xInit
      #self.x+ ~> int = xInit
      Base of Other:
        x: int = 2  # no * possible
      #var x*: int = xInit
      self.init()

      type
        self = object
          x {.private.} : int = xInit



class(Base):
  ctor(newBase) proc(a = 10, b = 20) =
    self.x* is int = a + 1
    self.y+ is int = b + 2
    self.p is string = "asdf"

  method baseMethod(z: int): int {.base.} =
    self.x + self.y + z

{.pop.}

block:
  let base = newBase(20)
  doAssert base.x == 21
  doAssert base.y == 22
  doAssert base.p == "asdf"


class(Sub of Base):
  ctor proc() =
    base(b = 200, a = 100)
    self.sub is string = "sub"

block:
  let base = Sub.init()
  doAssert base.x == 101
  doAssert base.y == 202
  doAssert base.p == "asdf"
  doAssert base.sub == "sub"


class(MyType):
  ctor proc() =
    self.someField is int = 0
    self.otherField is string = ""

block:
  # Would enable autocompletion for fields, but autocompletion for methods/procs still fails
  var self: MyType

  proc inc() =
    self.otherField = "asdf"

  proc getState(): int =
    #self.inc()
    self.someField

block:
  # This should enable full autocompletion
  # - The macro replaces Self with MyType everywhere
  # - The macro adds forward decls for all procs to make ordering non-significant
  type Self = MyType

  proc inc(self: Self) =
    self.otherField = "asdf"
    #self.inc()

  proc getState(self: Self): int =
    self.inc()
    self.someField = 1

#[
block:
  let b = newBase()
  doAssert b.getState() == 11
  doAssert b.getState() == 12
]#