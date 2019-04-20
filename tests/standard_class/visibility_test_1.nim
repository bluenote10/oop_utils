import visibility_lib
import ../test_utils

block:
  let t = newTest()

  assertCompileSuccess: t.a
  assertCompileSuccess: t.b
  assertCompileFailure: t.c

  assertCompileSuccess: t.a = 0
  assertCompileFailure: t.b = 0
  assertCompileFailure: t.c = 0

  doAssert t.a == 1
  doAssert t.b == 2

  t.a = 10
  doAssert t.a == 10
