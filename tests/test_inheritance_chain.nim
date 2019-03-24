import closure_methods

class(A, RootObj):
  constructor:
    proc newA*()

  var x = "a"

  proc a*() =
    echo x

class(AB, A):
  constructor:
    proc newAB*()

  base()

  var x = "b"

  proc b*() =
    echo x

class(ABC, AB):
  constructor:
    proc newABC*()

  base()

  var x = "c"

  proc c*() =
    echo x

block:
  let a = newA()
  a.a()

  let ab = newAB()
  ab.a()
  ab.b()

  let abc = newABC()
  abc.a()
  abc.b()
  abc.c()