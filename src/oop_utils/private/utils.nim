import macros
import options
import strformat


iterator items*[T](o: Option[T]): T =
  if o.isSome:
    yield o.get


proc expectKinds*(n: NimNode, kinds: set[NimNodeKind]) {.compileTime.} =
  ## checks that `n` is of kind `k`. If this is not the case,
  ## compilation aborts with an error message. This is useful for writing
  ## macros that check the AST that is passed to them.
  if not kinds.contains(n.kind): error("Expected a node of kinds " & $kinds & ", got " & $n.kind, n)


proc isCall*(n: NimNode): bool =
  n.kind == nnkCall

proc isCommand*(n: NimNode): bool =
  n.kind == nnkCommand

proc isStmtList*(n: NimNode): bool =
  n.kind == nnkStmtList

proc isVariableBinding*(n: NimNode): bool =
  n.kind in {nnkLetSection, nnkVarSection, nnkConstSection}

proc isAsgn*(n: NimNode): bool =
  n.kind == nnkAsgn

proc isAccQuoted*(n: NimNode): bool =
  n.kind == nnkAccQuoted

proc isIdent*(n: NimNode): bool =
  n.kind == nnkIdent

proc isIdent*(n: NimNode, s: string): bool =
  n.kind == nnkIdent and n.strVal == s


proc procBody*(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkTemplateDef, nnkLambda}
  n[n.len - 1]

proc `procBody=`*(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkTemplateDef, nnkLambda}
  expectKinds other, {nnkStmtList, nnkEmpty}
  n[n.len - 1] = other

proc procName*(n: NimNode): string =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkTemplateDef, nnkLambda}
  if n[0].isIdent:
    result = n[0].strVal
  elif n[0].kind == nnkPostfix and n[0][0].isIdent("*") and n[0][1].isIdent():
    result = n[0][1].strVal
  else:
    error &"Cannot infer proc name from {n.repr}", n

proc genericParams*(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkTemplateDef, nnkLambda}
  n[2]

proc `genericParams=`*(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkTemplateDef, nnkLambda}
  expectKind other, nnkGenericParams
  n[2] = other

proc formalParams*(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkTemplateDef, nnkLambda}
  n[3]

proc `formalParams=`*(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkTemplateDef, nnkLambda}
  expectKind other, nnkFormalParams
  n[3] = other

proc pragmas*(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkTemplateDef, nnkLambda}
  n[4]

proc `pragmas=`*(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkMethodDef, nnkTemplateDef, nnkLambda}
  expectKind other, nnkPragma
  n[4] = other

# -----------------------------------------------------------------------------
# Factories
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# Conversion
# -----------------------------------------------------------------------------

proc assumeStmtList*(n: NimNode): NimNode =
  if n.kind == nnkStmtList:
    n
  else:
    newStmtList(n)

proc convertProcDefIntoLambda*(n: NimNode): NimNode =
  result = newLambda()
  result.formalParams = n.formalParams
  result.procBody = n.procBody


proc toUntyped*(ast: NimNode): NimNode =
  # Replace NimIdent and NimSym by a fresh ident node
  proc inspect(node: NimNode): NimNode =
    case node.kind:
    of {nnkIdent, nnkSym}:
      return ident($node)
    of nnkEmpty:
      return node
    of nnkLiterals:
      return node
    else:
      var rTree = node.kind.newTree()
      for child in node:
        rTree.add inspect(child)
      return rTree
  result = inspect(ast)