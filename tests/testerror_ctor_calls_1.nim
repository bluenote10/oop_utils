#[
undeclared identifier: 'self'
]#
import closure_methods

class(Base):
  ctor(newBase)

  let x = self.initialize()

  proc initialize*(): int =
    x

block:
  let x = newBase()
