import closure_methods
import strformat

class(Counter):
  ctor(newCounter) proc(init: int)

  var counter = init

  proc inc*() = counter.inc
  proc dec*() = counter.dec
  proc get*(): int = counter

block:
  let counter = newCounter(0)
  counter.inc()
  echo counter.get()


classOf(DoubleCounter, Counter):
  ctor(newDoubleCounter)

  var counter = 100

  proc inc*() = counter += 2
  proc dec*() = counter -= 2
  proc get*(): int = counter

block:
  let counter: Counter = newDoubleCounter()
  counter.inc()
  echo counter.get()


when false:
  class(AbstractInterface):
    proc toImplementA*(): string
    proc toImplementB*(): string
    proc compute*(): string =
      self.toImplementA() & self.toImplementB()

  classOf(X, AbstractInterface):
    base()
    proc toImplementA*(): string = "A"
    proc toImplementB*(): string = "B"

  block:
    let x = X.init()
    doAssert x.compute() == "AB"

when true:
  class(AbstractInterface):
    ctor proc(prefix: string)
    proc toImplementA*(): string
    proc toImplementB*(): string
    proc compute*(): string =
      prefix & self.toImplementA() & self.toImplementB()

  classOf(X, AbstractInterface):
    base("x_prefix_")
    proc toImplementA*(): string = "A"
    proc toImplementB*(): string = "B"

  block:
    let x = X.init()
    echo x.compute()
