import oop_utils/closure_class

class(Base):
  ctor(newBase) proc(id: string)
  proc getId*(): string = id

class(Sub of Base):
  let id = "sub"
  base(id)

let x = Sub.init()
doAssert x.getId() == "sub"
