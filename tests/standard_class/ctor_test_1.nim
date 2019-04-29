import oop_utils/standard_class

class(NoCtorBody):
  ctor(newNoCtorBody)

block:
  doAssert newNoCtorBody().isNil == false


class(NoProc):
  ctor(newNoProc):
    self:
      a = 1

block:
  doAssert newNoProc().a == 1


class(EmptyProc):
  ctor(newEmptyProc) proc () =
    self:
      a = 1

block:
  doAssert newEmptyProc().a == 1
