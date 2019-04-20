#[
undeclared identifier: 'self'
]#
import oop_utils/closure_class

class(Base):
  ctor(newBase)

  let x = self.initialize()

  proc initialize*(): int =
    x

block:
  let x = newBase()
