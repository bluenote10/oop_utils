import oop_utils/standard_class
import strformat
import tables


type
  Generic[A, B] = ref object


proc newGeneric[A, B](): Generic[A, B] =
  Generic[A, B]()

proc newGeneric[A, B](a: A, b: B): Generic[A, B] =
  Generic[A, B]()


class(Test):
  ctor proc() =
    let k = 1
    let v = "value"
    self:
      a = initTable[int, string]()
      b = initTable[int, string]()
      c = newGeneric[Generic[int, int], Generic[int, int]]()
      d = newGeneric(k, v)

