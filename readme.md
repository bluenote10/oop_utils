# oop_utils [![Build Status](https://github.com/bluenote10/oop_utils/workflows/ci/badge.svg)](https://github.com/bluenote10/oop_utils/actions?query=workflow%3Aci) [![license](https://img.shields.io/github/license/mashape/apistatus.svg)](LICENSE) <a href="https://github.com/yglukhov/nimble-tag"><img src="https://raw.githubusercontent.com/yglukhov/nimble-tag/master/nimble.png" height="23" ></a>

oop_utils provides macros that allow to easily create OOP class hierarchies.

It comes in two different flavors:
- [Standard classes](readme_standard_class.md): Allows to define type hierarchies based on Nim's standard method dispatch.
- [Closure classes](readme_closure_class.md): Allows to define type hierarchies based on closure method dispatch.

The two approaches have minor syntactical differences, but share the same general scheme. For comparison:

```nim
import oop_utils/standard_class

# A standard class:
# - The object instance is attached to a `self` symbol.
# - Initialization + field definitions go into the ctor proc.
class(Counter):
  ctor(newCounter) proc(init: int) =
    self:
      counter = init

  method inc*() {.base.} = self.counter.inc
  method dec*() {.base.} = self.counter.dec
  method get*(): int {.base.} = self.counter
```

vs.

```nim
import oop_utils/closure_class

# A closure class:
# - No `self` symbol required.
# - Initialization + field definitions go into main scopre to emphasize closure nature.
class(Counter):
  ctor(newCounter) proc(init: int)

  var counter = init

  proc inc*() = counter.inc
  proc dec*() = counter.dec
  proc get*(): int = counter
```
