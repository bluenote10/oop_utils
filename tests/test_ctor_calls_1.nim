import closure_methods

var global = ""

class(Test):
  ctor(newTest)

  echo "initializing"
  global = "initialized"


let t = newTest()
doAssert global == "initialized"