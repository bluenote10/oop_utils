import oop_utils/closure_class

class(A):
  ctor(newA) proc()

  proc a*(): string = "a"

class(AB of A):
  ctor(newAB) proc()

  base()

  proc b*(): string = "b"

class(ABC of AB):
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