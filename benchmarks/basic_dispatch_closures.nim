import closure_methods
import times
import strformat

class(Base):
  ctor(newBase) proc(xInit: int)

  var x = xInit

  proc add*(value: int): int =
    x + value

class(A of Base):
  ctor(newA)
  base(1)

class(B of Base):
  ctor(newB)
  base(2)

class(C of Base):
  ctor(newC)
  base(3)

class(D of Base):
  ctor(newD)
  base(4)

class(E of Base):
  ctor(newE)
  base(5)

class(F of Base):
  ctor(newF)
  base(6)

class(G of Base):
  ctor(newG)
  base(7)

class(H of Base):
  ctor(newH)
  base(8)

class(I of Base):
  ctor(newI)
  base(9)

class(J of Base):
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
