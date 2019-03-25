import closure_methods
import times
import strformat

class(Base):
  ctor(newBase) proc(xInit: int)

  var x = xInit

  proc add*(value: int): int =
    x + value

classOf(A, Base):
  ctor(newA)
  base(1)

classOf(B, Base):
  ctor(newB)
  base(2)

classOf(C, Base):
  ctor(newC)
  base(3)

classOf(D, Base):
  ctor(newD)
  base(4)

classOf(E, Base):
  ctor(newE)
  base(5)

classOf(F, Base):
  ctor(newF)
  base(6)

classOf(G, Base):
  ctor(newG)
  base(7)

classOf(H, Base):
  ctor(newH)
  base(8)

classOf(I, Base):
  ctor(newI)
  base(9)

classOf(J, Base):
  ctor(newJ)
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
