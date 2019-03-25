import closure_methods

class(X):
  constructor:
    proc newX()

  let a = 1
  var b = 2
  var c = 3

  getter[int](a)
  setter[int](b)
  getterSetter[int](c)

  getter[int](a, getCustomA)
  setter[int](b, setCustomB)
  getterSetter[int](c, getCustomC, setCustomC)

block:
  let x = newX()

  assert x.getA() == 1

  x.setB(0)

  assert x.getC() == 3
  x.setC(4)
  assert x.getC() == 4

block:
  let x = newX()

  assert x.getCustomA() == 1

  x.setCustomB(0)

  assert x.getCustomC() == 3
  x.setCustomC(4)
  assert x.getCustomC() == 4

