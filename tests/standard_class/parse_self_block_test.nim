include oop_utils/standard_class

#[
macro parseFields(body: untyped): typed =
  var ctor = Constructor()
  parseSelfBlock(body, ctor)
  result = newNimNode(nnkBracketExpr)
  for field in ctor.fields:
    result.add(newCall "field", [field.name])#, field.access, field.rhs, field.typ])
  #result = ctor.fields
  echo result.repr


static:
  block:
    let fields = parseFields:
      x
      y

    doAssert fields == @[
      field("x", Access.Private, ident "x"),
      field("y", Access.Private, ident "y")
    ]
]#


macro toNimNode(body: untyped): NimNode =
  ## This doesn't work because the AST gets semchecked on return,
  ## that's why quote is a magic.
  result = body.copy
  echo result.treerepr

proc validateFields(expected: seq[Field], body: NimNode) =
  var ctor = Constructor()
  parseSelfBlock(body.assumeStmtList(), ctor)
  let actual = ctor.fields

  template check(cond: bool) =
    if not cond:
      echo "expected:\n", expected.repr
      echo "actual:\n", actual.repr
    doAssert cond

  check expected.len == actual.len
  for i in 0 ..< expected.len:
    check actual[i].name == expected[i].name
    check actual[i].access == expected[i].access
    check actual[i].rhs == expected[i].rhs
    check actual[i].typ == expected[i].typ


macro self(body: untyped): untyped =
  ## That also doesn't work. Probably because the argument to quote is `typed`
  ## and the body will not pass the semchecker...
  result = newCall(ident "quote", [body, newStrLitNode("@@")])
  echo result.repr


static:
  validateFields(@[
      field("x", Access.Private, ident "x"),
  ]):
    quote:
      x

  validateFields(@[
      field("x", Access.Private, ident "x"),
      field("y", Access.Private, ident "y"),
  ]):
    quote:
      x
      y

  #[
  validateFields(@[
      field("a", Access.Private, ident "a"),
      field("b", Access.Readable, ident "b"),
      field("c", Access.Public, ident "c"),
  ]):
    quote:
      a
      b `+`
      c `*`
  ]#
