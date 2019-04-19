import oop_utils/unwrap

import macros
import options
import sequtils


type
  Ident = object
    name: string


proc unwrapImpl(T: typedesc[Ident], n: NimNode): Option[Ident] =
  if n.kind == nnkIdent:
    some(Ident(name: n.strVal))
  else:
    none(Ident)


type
  VarDef = object
    lhs: string
    rhs: NimNode
    pragmas: seq[NimNode]

proc unwrapImpl(T: typedesc[VarDef], n: NimNode): Option[VarDef] =
  if n.kind == nnkVarSection:
    if n.len != 1:
      return none(VarDef)
    let firstIdentDef = n[0]
    if firstIdentDef[0].kind in {nnkIdent, nnkSym}:
      return some(VarDef(lhs: firstIdentDef[0].strVal, rhs: firstIdentDef[2]))
    elif firstIdentDef[0].kind == nnkPragmaExpr:
      let pragmas = toSeq(firstIdentDef[0][1].children)
      return some(VarDef(lhs: firstIdentDef[0][0].strVal, rhs: firstIdentDef[2], pragmas: pragmas))
    return none(VarDef)
  else:
    return none(VarDef)


static:
  block:
    let nodes = [
      newEmptyNode(),
      quote do:
        someIdent
      ,
      quote do:
        var x = 1
      ,
      quote do:
        var x {.withPragma.} = 1
      ,
    ]
    for n in nodes:
      # statement test
      unwrap(n):
        i @ Ident:
          echo "Ident: " & i.repr
        v @ VarDef:
          echo "VarDef: " & v.repr
        _:
          echo "no match"

      # expression test
      let value = unwrap(n):
        i @ Ident:
          "Ident: " & i.repr
        v @ VarDef:
          "VarDef: " & v.repr
        _:
          "no match"
      echo value

      # n.unwrap((i @ Ident: echo i))

type
  Some = object

proc unwrapImpl[X](T: typedesc[Some], o: Option[X]): Option[X] = o

block:
  let options = [some(1), none(int), some(2)]
  for o in options:
    unwrap(o):
      x @ Some:
        echo "some(" & $x & ")"
      _:
        echo "none"

