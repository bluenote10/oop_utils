#[
type mismatch: got <B, OtherBase>
]#
import oop_utils/standard_class
import strformat

type
  OtherBase = ref object of RootObj
  OtherSub = ref object of OtherBase

class(A):
  ctor proc(ob: OtherBase) =
    self:
      ob

class(B of A):
  ctor proc(os: OtherSub) =
    self:
      base(os)
      os

block:
  let a = A.init(OtherBase())
  let b = B.init(OtherSub())

class(C of B):
  ctor proc() =
    # We have to be careful here: The base call uses the type defined
    # in the ctor of A, not the ctor of B. Because the upcast from C
    # to B still matches the first patch argument of the A ctor (a B is
    # an A), the ctor of A may match...
    let accidentalOtherBase = OtherBase()
    self:
      base(accidentalOtherBase)


block:
  let a = A.init(OtherBase())
  let b = B.init(OtherSub())
  let c = C.init()

  doAssert not a.ob.isNil

  doAssert not b.ob.isNil
  doAssert not b.os.isNil

  doAssert not c.ob.isNil
  doAssert not c.os.isNil

