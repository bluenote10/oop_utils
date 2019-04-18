import oop_utils/standard_class


class(Base):
  ctor(newBase) proc(a = 10, b = 20) =
    self:
      x`*` = a + 1
      y`+` = b + 2
      p = "asdf"

  method baseMethod(z: int): int {.base.} =
    self.x + self.y + z

block:
  let base = newBase(20)
  doAssert base.x == 21
  doAssert base.y == 22
  doAssert base.p == "asdf"


class(Sub of Base):
  ctor proc() =
    self:
      base(b = 200, a = 100)
      sub = "sub"

block:
  let base = Sub.init()
  doAssert base.x == 101
  doAssert base.y == 202
  doAssert base.p == "asdf"
  doAssert base.sub == "sub"


class(MyType):
  ctor proc() =
    self:
      someField = 0
      otherField = ""

  # Syntactically most concise, but no autocompletion here...
  proc inc() =
    self.otherField = "asdf"

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

