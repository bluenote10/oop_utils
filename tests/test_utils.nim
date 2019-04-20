
template assertCompileSuccess*(body: untyped): untyped =
  doAssert(compiles(body))

template assertCompileFailure*(body: untyped): untyped =
  doAssert(not(compiles(body)))

assertCompileSuccess:
  let x = 1
  echo x

assertCompileFailure:
  let x = 1
  echo y
