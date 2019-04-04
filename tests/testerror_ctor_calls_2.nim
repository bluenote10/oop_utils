#[
undeclared identifier: 'private'
]#
import closure_methods

class(Base):
  ctor(newBase)

  proc private(): int = self.initialize()

  let x = private()

  proc initialize*(): int =
    x

block:
  let x = newBase()
