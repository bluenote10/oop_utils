import oop_utils/closure_class
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


class(DoubleCounter of Counter):
  ctor(newDoubleCounter)

  var counter = 100

  proc inc*() {.override.} = counter += 2
  proc dec*() {.override.} = counter -= 2
  proc get*() : int {.override.} = counter

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

  class(X of AbstractInterface):
    base()
    proc toImplementA*(): string {.override.} = "A"
    proc toImplementB*(): string {.override.} = "B"

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

  class(X of AbstractInterface):
    base("x_prefix_")
    proc toImplementA*(): string {.override.} = "A"
    proc toImplementB*(): string {.override.} = "B"

  block:
    let x = X.init()
    echo x.compute()
