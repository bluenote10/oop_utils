import oop_utils/standard_class

template otherBlock(body) =
  body

class(RobustFindSelfBlock):
  ctor proc() =
    block:
      echo "test"
    otherBlock:
      echo "test"
    self:
      x = 1

block:
  let x = RobustFindSelfBlock.init()
