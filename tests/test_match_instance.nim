import closure_methods/match_instance

type
  Base = ref object of RootObj
  A = ref object of Base
  B = ref object of Base

proc a(x: A): string = "A"
proc b(x: B): string = "B"

let elems = [A().Base, B()]

for x in elems:
  let expected =
    if x of A:
      "A"
    else:
      "B"

  var sideEffect: string

  matchInstance:
    case x
    of A:
      sideEffect = x.a()
    of B:
      sideEffect = x.b()
    else:
      sideEffect = "nothing"

  doAssert sideEffect == expected

  let val = matchInstance:
    case x
    of A:
      echo "A"
      echo x.a()
      x.a()
    of B:
      echo "B"
      echo x.b()
      x.b()
    else:
      "asdf"

  doAssert val == expected
