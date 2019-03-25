import closure_methods

class(A):
  ctor(newA) proc()

  proc a*(): string = "a"

classOf(AB, A):
  ctor(newAB) proc()

  base()

  proc b*(): string = "b"

classOf(ABC, AB):
  ctor(newABC) proc()

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