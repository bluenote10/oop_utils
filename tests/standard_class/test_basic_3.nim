import oop_utils/standard_class

class(Counter):
  ctor(newCounter) proc(init: int) =
    self:
      counter = init

  method inc*() {.base.} = self.counter.inc
  method dec*() {.base.} = self.counter.dec
  method get*(): int {.base.} = self.counter

block:
  let counter = newCounter(0)
  counter.inc()
  echo counter.get()


class(DoubleCounter of Counter):
  ctor(newDoubleCounter) proc() =
    self:
      base(100)

  method inc*() = self.counter += 2
  method dec*() = self.counter -= 2
  method get*() : int = self.counter

block:
  let counter: Counter = newDoubleCounter()
  counter.inc()
  echo counter.get()

