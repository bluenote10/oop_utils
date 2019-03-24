import closure_methods

class(A):
  constructor:
    proc newA*()

  proc a*(): string = "a"

classOf(AB, A):
  constructor:
    proc newAB*()

  base()

  proc b*(): string = "b"

classOf(ABC, AB):
  constructor:
    proc newABC*()

  base()

  proc c*(): string = "c"


block:
  let a = newA()
  doAssert a.a() == "a"

  let ab = newAB()
  doAssert ab.a() == "a"
  doAssert ab.b() == "b"

  let abc = newABC()
  doAssert abc.a() == "a"
  doAssert abc.b() == "b"
  doAssert abc.c() == "c"