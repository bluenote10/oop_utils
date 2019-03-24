import closure_methods
import times
import strformat

class(Base):
  constructor:
    proc newBase*(xInit: int)

  var x = xInit

  proc add*(value: int): int =
    x + value

classOf(A, Base):
  constructor:
    proc newA*()
  base(1)

classOf(B, Base):
  constructor:
    proc newB*()
  base(2)

classOf(C, Base):
  constructor:
    proc newC*()
  base(3)

classOf(D, Base):
  constructor:
    proc newD*()
  base(4)

classOf(E, Base):
  constructor:
    proc newE*()
  base(5)

classOf(F, Base):
  constructor:
    proc newF*()
  base(6)

classOf(G, Base):
  constructor:
    proc newG*()
  base(7)

classOf(H, Base):
  constructor:
    proc newH*()
  base(8)

classOf(I, Base):
  constructor:
    proc newI*()
  base(9)

classOf(J, Base):
  constructor:
    proc newJ*()
  base(10)

block:
  let numIter = 123_456

  let a = newA()
  let b = newB()
  let c = newC()
  let d = newD()
  let e = newE()
  let f = newF()
  let g = newG()
  let h = newH()
  let i = newI()
  let j = newJ()

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
