#[
undeclared identifier: 'private'
]#
import oop_utils/closure_class

class(Base):
  ctor(newBase)

  proc private(): int = self.initialize()

  let x = private()

  proc initialize*(): int =
    x

block:
  let x = newBase()
