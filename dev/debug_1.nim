import macros

when false:

  macro doubleBody(body1: untyped, body2: typed): untyped =
    #echo "\nbody1:"
    #echo body1.repr
    #echo "\nbody2:"
    #echo body2.repr

    echo body2.treeRepr

    result = newStmtList()
    #result.add(newBlockStmt(body2))
    #result.add(newVarStmt(ident "y", body2[0][0][2]))
    result.add(newVarStmt(ident "y", body2[0][6][0][2]))
    result.add(newBlockStmt(body1))

    #result.add(body2)
    #result.add(body1)

    echo result.repr


  macro singleBody(body: untyped): untyped =
    #echo body.treeRepr
    result = newCall(bindSym "doubleBody", body, body)

    let x = 1

    singleBody():
      proc test() =
        let z = x
        #echo x

    #test(42)


when false:
  macro takesTyped(t: typed): untyped =
    result = newStmtList(
      newVarStmt(ident "y", t),
      newCall(ident "echo", t),
    )
    result[0][0][1] = t.getType
    echo result.repr

  var x = 1
  takesTyped(x)


when false:
  macro doubleBody(body1: untyped, body2: typed): untyped =
    #echo "\nbody1:"
    #echo body1.repr
    #echo "\nbody2:"
    #echo body2.repr

    echo body2.treeRepr
    let t = body2[0][3][1][1]
    echo t.treeRepr

    let binding = newStmtList(
      newVarStmt(ident "a", newEmptyNode()),
      #newCall(ident "echo", t),
    )
    binding[0][0][1] = t.getType

    template objectDef(fieldType): untyped {.dirty.} =
      type
        Object = ref object of RootObj
          x: fieldType


    result = newStmtList()
    result.add(binding)
    result.add(getAst(objectDef(t.getType)))
    result.add(body1)

    #result.add(body2)
    #result.add(body1)

    echo result.repr


  macro singleBody(body: untyped): untyped =
    #echo body.treeRepr
    result = newCall(bindSym "doubleBody", body, body)

  #let x = 1
  singleBody():
    proc test(x: int) = echo x



when false:
  # This fails because
  # - the "typedef" uses a new ident
  # - the "usage" uses the old symbol

  macro typedMacro(body: typed): untyped =
    result = newProc(
      ident "newTest",
      [newEmptyNode(), newIdentDefs(ident "x", ident "int")],
      body[0][6], # the original proc body
    )
    echo result.treerepr

  typedMacro():
    proc test(x: int) =
      echo x

  newTest(1)


when false:

  macro typedMacro(body: typed): untyped =
    #echo body.treeRepr
    result = newStmtList()
    result.add(newLetStmt(ident "x", newIntLitNode(1)))
    result.add(body[0][1][1])
    echo result.treerepr

  typedMacro():
    block:
      let x = 1
      echo x


when false:
  # This works because it is the other way around:
  # - the "typedef" uses the old symbol
  # - the "usage" uses a new ident

  macro typedMacro(body: typed): untyped =
    #echo body.treeRepr
    result = newStmtList(body, newCall(ident "echo", ident "x"))
    echo result.treerepr

  typedMacro():
    var x = 1


when false:

  macro typedMacro(body: typed): untyped =
    result = newCall(ident "echo", newStrLitNode "Hello World")

  macro untypedMacro(body: untyped): untyped =
    result = newCall(ident "typedMacro", body)

  untypedMacro():
    echo "here"
    proc test(x: int) =
      echo x

  test(1)


when true:
  macro takesTwo(a, b: typed): untyped =
    result = newStmtList()

  takesTwo() do :
    let x = 1
  do:
    x