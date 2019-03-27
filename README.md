# closure_methods [![Build Status](https://travis-ci.org/bluenote10/closure_methods.svg?branch=master)](https://travis-ci.org/bluenote10/closure_methods) [![license](https://img.shields.io/github/license/mashape/apistatus.svg)](LICENSE) <a href="https://github.com/yglukhov/nimble-tag"><img src="https://raw.githubusercontent.com/yglukhov/nimble-tag/master/nimble.png" height="23" ></a>

closure_methods provides a macro that allows to easily create OOP class hierarchies.
It is based on a basic design decision:

> All object fields are closures

The library was written with the JS target in mind, but works fine on C/C++ targets as well.

The design decision to model methods as closures comes at the price that every data access has to go through getters/setters. On the other hand, it has a few interesting properties:
- The direct consequence: Full encapsulation. In contrast to regular object fields it is much easier to expose only what has to be exposed.
- Internal state can be split into mutable/immutable via `var`/`let` bindings. For example
    ```nim
    class(Person):
      let name = "John Doe"
      var age = 42
      # methods...
    ```
    It is clear what can/cannot change about an object.
- In contrast to having explicit object fields, there are no name conflicts in class hierarchies. For instance:
    ```nim
    class(UiComponent):
      var state: SomeRenderState
      # (abstract) methods...
    
    classOf(UiCheckboxComponent, UiComponent):
      var state: bool # doesn't matter that a parent also uses `state`
      # methods...
    ```

The secondard goal was to make the DSL as much DRY as as possible.


## Examples

### Basic class

```nim
class(Counter):
  ctor(newCounter) proc(init: int)

  var counter = init

  proc inc*() = counter.inc
  proc dec*() = counter.dec
  proc get*(): int = counter

block:
  let counter = newCounter(0)
  counter.inc()
  echo counter.get()
```

**Note on getters/setter**: Because field access has to go through procs, the DSL provides convenience syntax for that. We could use something like:
```nim
class(Counter):
  ctor(newCounter) proc(init: int)

  var counter = init
  
  getter[int](counter)
  setter[int](counter)
  # or as a shortcut for the two:
  getterSetter[int](counter)
```

The default names for the getter/setters are to prefix the variable with `get`/`set`, so `getCounter`/`setCounter` in this example.
It is also possible to use `getter[int](counter, myCustomGetter)` or `getterSetter[int](counter, myCustomGetter, myCustomSetter)` to modify the accessor names.


### Derived class

Derived classes are created with the `classOf` variant of the macro to specify the base type:

```nim
classOf(DoubleCounter, Counter):
  ctor(newDoubleCounter)

  var counter = 100

  proc inc*() = counter += 2
  proc dec*() = counter -= 2
  proc get*(): int = counter

block:
  let counter: Counter = newDoubleCounter()
  counter.inc()
  echo counter.get()
```

**Note on constructors**: If the constructor does not take parameters, we can omit the `proc` part.
In fact, the macro also generates generic constructors according to [Nim RFC 48](https://github.com/nim-lang/RFCs/issues/48) automatically and we could construct the object like this:

```nim
let c1 = Counter.init(10)
let c2 = DoubleCounter.init()
```

If there is no need for generating named constructors, we could simplify the ctor statements further to `ctor proc(init: int)` or omit it entirely if it takes no parameters.


### Interfaces

The macro understands abstract methods and helps to validate if subclasses implement all abstract methods. For example:

```nim
class(AbstractInterface):
  proc toImplementA*(): string
  proc toImplementB*(): string
  proc compute*(): string =
    self.toImplementA() & self.toImplementB()
```

Note: `self` is the symbol injected by the macro that refers to the class instance, and has to be used to call methods from other methods.

If a proc has no body, the macro treats it as abstract. If a class has abstract methods, the entire class is considered abstract, and there won't be public constructors (the macro will reject attempts to create a named constructor). The following class is non-abstract, because it overloads all abstract base methods:

```nim
classOf(X, AbstractInterface):
  base()
  proc toImplementA*(): string = "A"
  proc toImplementB*(): string = "B"
  
block:
  let x = X.init()
  doAssert x.compute() == "AB"
```

Note that there is a call to `base()`, which initializes the base class, in this case the `self.compute` method.
In general the `base(...)` call takes the arguments of the parent constructor. We could modify the example to:

```nim
class(AbstractInterface):
  ctor proc(prefix: string) # only available from subclasses, because class is abstract
  proc toImplementA*(): string
  proc toImplementB*(): string
  proc compute*(): string =
    prefix & self.toImplementA() & self.toImplementB()

classOf(X, AbstractInterface):
  base("x_prefix_")
  proc toImplementA*(): string = "A"
  proc toImplementB*(): string = "B"
```

When there is a base class, the macro injects a symbol `base` similar to `self`. 
The `base` symbol allows to make explicit calls to the base class, i.e., if the
class overloads `someMethod`, it could call `base.someMethod` in the implementation
of `self.someMethod`.

## DSL rules

The macro supports the following syntax within the body of `class`/`classOf`.

#### 1. Constructor

The syntax is either
```nim
ctor(newX) proc(args...)
```
to create a generic + a named constructor, or
```nim
ctor proc(args...)
```
to create only the generic constructor.
In the spirit of DRY, it is not necessary to put the class type itself as a return type of the `proc` -- the macro takes care of that internally.

#### 2. Base call

A syntax of
```nim
base(args)
```
is transformed into a call of the base class constructor.

#### 3. Variable definitions

Any `var`/`let`/`const` statement is placed at the top of resulting closure generating function.

#### 4. Procs

Procs can be devided into:
- procs without an export `*`: These private procs are places below the variable definitions in the resulting closure generating function.
- procs with an export `*` and body: These are regular "methods". They will be turned into object fields and are available as `self.methodName`.
- procs with an export `*` but no body: These are abstract methods.

#### 5. Getter/Setter

As a convenience the following syntax is supported to easily access internal state:
```nim
getter[T](field)
setter[T](field)
getterSetter[T](field)
getter[T](field, getterMethodName)
setter[T](field, setterMethodName)
getterSetter[T](field, getterMethodName, setterMethodName)
```

### Under the hood

To get an idea of what the macro is generating, this is the code produced from the `AbstractInterface` example (slightly edited):

```nim
# The base class: Because it is abstract, the macro only generate 
# the type and `patch` function, which is kind of an internal 
# constructor.
type
  AbstractInterface* = ref object of RootObj
    # The `used` pragma is currently abused to mark abstract methods, because
    # there are some bugs with custom annotations
    toImplementA* {.used.}: proc (): string
    toImplementB* {.used.}: proc (): string
    compute*: proc (): string

proc patch*(self: AbstractInterface): proc (prefix: string) =
  result = proc (prefix: string) =
    # Note that the macro automatically generates meaningful
    # bodies for the abstract methods
    self.toImplementA = proc (): string =
      doAssert(false, "called abstract method \'toImplementA\'")
    self.toImplementB = proc (): string =
      doAssert(false, "called abstract method \'toImplementB\'")
    self.compute = proc (): string =
      prefix & self.toImplementA() & self.toImplementB()

# Now the derived type...
type
  X* = ref object of AbstractInterface
  
proc patch*(self: X): proc () =
  result = proc () =
    # Note that this calls the `patch` proc above, which
    # means that the fields are initialized from the base
    # class. In particular, this sets `self.compute`
    # properly
    patch(AbstractInterface(self))("x_prefix_")
    
    # The macro injects a `base` symbol similar to `self`
    # to allow explicit base calls
    var base = AbstractInterface()
    base.toImplementA = self.toImplementA
    base.toImplementB = self.toImplementB
    base.compute = self.compute
    
    # Now come the actual overloads
    self.toImplementA = proc (): string =
      "A"
    self.toImplementB = proc (): string =
      "B"

# This function only has a generic constructor because
# we didn't use a named ctor. If we would add a named
# ctor there would be just another proc like this...
proc init*(:tmp339068: typedesc[X]): X =
  var self = X()
  patch(self)()
  self
```
