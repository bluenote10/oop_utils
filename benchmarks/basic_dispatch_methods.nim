import oop_utils/closure_class
import times
import strformat

type
  Base = ref object of RootObj
    x: int

  A = ref object of Base
  B = ref object of Base
  C = ref object of Base
  D = ref object of Base
  E = ref object of Base
  F = ref object of Base
  G = ref object of Base
  H = ref object of Base
  I = ref object of Base
  J = ref object of Base



method add(self: Base, value: int): int {.base.} =
  self.x + value

block:
  let numIter = 123_456

  let a = A(x: 1)
  let b = B(x: 2)
  let c = C(x: 3)
  let d = D(x: 4)
  let e = E(x: 5)
  let f = F(x: 6)
  let g = G(x: 7)
  let h = H(x: 8)
  let i = I(x: 9)
  let j = J(x: 10)

  let all = [a.Base, b, c, d, e, f, g, h, i, j]

  let t1 = now()
  var
    values = 0
    counts = 0
  for i in 1 .. numIter:
    for x in all:
      values += x.add(1)
      counts += 1
  echo values
  let t2 = now()
  let delta = (t2 - t1).inMicroseconds.float / 1000d
  echo &"Time for {counts} function dispatches: {delta:.1f} ms"
  echo &"Time for single function dispatch: {delta / counts.float * 1_000_000:.1f} ns"
