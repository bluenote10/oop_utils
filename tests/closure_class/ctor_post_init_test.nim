import oop_utils/closure_class

class(Counter):
  var count = 42
  let id {.used.} = "id"

  proc reset*() =
    count = 0
  proc get*(): int = count

  postInit:
    self.reset()
    let id = 42 # shadowing possible?


let c = Counter.init()
doAssert c.get() == 0
