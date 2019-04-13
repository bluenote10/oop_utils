import macros
import options


iterator items*[T](o: Option[T]): T =
  if o.isSome:
    yield o.get


proc expectKinds*(n: NimNode, kinds: set[NimNodeKind]) {.compileTime.} =
  ## checks that `n` is of kind `k`. If this is not the case,
  ## compilation aborts with an error message. This is useful for writing
  ## macros that check the AST that is passed to them.
  if not kinds.contains(n.kind): error("Expected a node of kinds " & $kinds & ", got " & $n.kind, n)

proc isIdent*(n: NimNode, s: string): bool =
  n.kind == nnkIdent and n.strVal == s


proc procBody*(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkLambda}
  n[n.len - 1]

proc `procBody=`*(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkLambda}
  expectKinds other, {nnkStmtList, nnkEmpty}
  n[n.len - 1] = other

proc genericParams*(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef}
  n[2]

proc `genericParams=`*(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkLambda}
  expectKind other, nnkGenericParams
  n[2] = other

proc formalParams*(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkLambda}
  n[3]

proc `formalParams=`*(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkLambda}
  expectKind other, nnkFormalParams
  n[3] = other

proc pragmas*(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkLambda}
  n[4]

proc `pragmas=`*(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkLambda}
  expectKind other, nnkPragma
  n[4] = other

proc publicIdent*(s: string): NimNode =
  newNimNode(nnkPostfix).add(
    ident "*",
    ident s,
  )

proc newLambda*(): NimNode =
  newNimNode(nnkLambda).add(
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode(),
    newNimNode(nnkFormalParams),
    newEmptyNode(),
    newEmptyNode(),
    newStmtList(),
  )

proc convertProcDefIntoLambda*(n: NimNode): NimNode =
  result = newLambda()
  result.formalParams = n.formalParams
  result.procBody = n.procBody
