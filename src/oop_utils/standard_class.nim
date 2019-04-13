import macros
import strformat
import options
import sets
import strutils
import sequtils

import private/utils

import oop_utils/match_instance
export match_instance

# -----------------------------------------------------------------------------
# Class name parsing (needed to split generic params)
# -----------------------------------------------------------------------------

type
  ClassDef = ref object
    name: string
    rawClassDef: NimNode
    identClass: NimNode
    genericParams: NimNode
    #identBaseClass: NimNode

proc parseClassName(classDef: ClassDef, n: NimNode) =
  ## Helper function to split the class ident from generic params
  if n.kind == nnkIdent:
    classDef.identClass = n
    classDef.genericParams = newEmptyNode()
  elif n.kind == nnkBracketExpr:
    let identClass = n[0]
    let genericParams = newNimNode(nnkGenericParams)
    for i in 1 ..< n.len:
      genericParams.add(
        newIdentDefs(n[i], newEmptyNode(), newEmptyNode())
      )
    classDef.identClass = identClass
    classDef.genericParams = genericParams
  else:
    error &"Cannot parse class definition: {n.repr}", n

proc parseDefinition(n: NimNode): ClassDef =
  result = ClassDef()
  result.rawClassDef = n
  result.parseClassName(n)
  result.name = result.identClass.strVal


# -----------------------------------------------------------------------------
# Base call parsing
# -----------------------------------------------------------------------------

type
  ParsedBaseCall = ref object
    args: seq[NimNode]
    origNode: NimNode

proc isBaseCall(n: NimNode): bool =
  n.kind == nnkCall and n[0].kind == nnkIdent and n[0].strVal == "base"

proc parseBaseCall(n: NimNode): ParsedBaseCall =
  var args = newSeq[NimNode]()
  for i in 1 ..< n.len:
    args.add(n[i])
  ParsedBaseCall(args: args, origNode: n)


# -----------------------------------------------------------------------------
# Constructor parsing
# -----------------------------------------------------------------------------

type
  BodyStmt = ref object of RootObj
  BodyStmtNode = ref object of BodyStmt
    node: NimNode
  BodySelfAsgn = ref object of BodyStmt
    field: string
    rhs: NimNode
  BodyStmtBaseCall = ref object of BodyStmt
    baseCall: ParsedBaseCall


type
  Access = enum
    Public, Readable, Private

  Field = ref object
    name: string
    access: Access
    typ: NimNode

  Constructor = ref object
    name: Option[string]
    args: seq[NimNode]
    fields: seq[Field]
    body: seq[BodyStmt]
    hasBaseCall: bool

proc isConstructor(n: NimNode): bool =
  # case1 matches:
  # - ctor proc(...)
  # - ctor(named) proc(...)
  let case1 = n.kind == nnkCommand and (
    (n[0].kind == nnkIdent and n[0].strVal == "ctor") or
    (n[0].kind == nnkCall and n[0][0].kind == nnkIdent and n[0][0].strVal == "ctor")
  )
  # case2 matches ctor without arguments:
  # - ctor(named)
  let case2 = n.kind == nnkCall and n[0].kind == nnkIdent and n[0].strVal == "ctor"
  return case1 or case2

proc parseConstructorBody(ctorBody: NimNode, ctor: Constructor) =
  #[
    Asgn
      Infix
        Ident "*"
        DotExpr
          Ident "self"
          Ident "x"
        Prefix
          Ident "is"
          Ident "int"
      Ident "xInit"
    Asgn
      Infix
        Ident "+"
        DotExpr
          Ident "self"
          Ident "y"
        Prefix
          Ident "is"
          Ident "int"
      Ident "xInit"
    Asgn
      Infix
        Ident "is"
        DotExpr
          Ident "self"
          Ident "p"
        Ident "string"
      StrLit "asdf"
  ]#
  for n in ctorBody:
    if n.kind == nnkAsgn and n[0].kind == nnkInfix:
      let infix = n[0]
      let rhs = n[1]
      if infix[0].isIdent("*") and infix[1].kind == nnkDotExpr and
         infix[1][0].isIdent("self") and infix[2].kind == nnkPrefix and infix[2][0].isIdent("is"):
          let name = infix[1][1].strVal
          let typ = infix[2][1]
          ctor.fields.add(Field(name: name, access: Access.Public, typ: typ))
          ctor.body.add(BodySelfAsgn(field: name, rhs: rhs))
      elif infix[0].isIdent("+") and infix[1].kind == nnkDotExpr and
           infix[1][0].isIdent("self") and infix[2].kind == nnkPrefix and infix[2][0].isIdent("is"):
          let name = infix[1][1].strVal
          let typ = infix[2][1]
          ctor.fields.add(Field(name: name, access: Access.Readable, typ: typ))
          ctor.body.add(BodySelfAsgn(field: name, rhs: rhs))
      elif infix[0].isIdent("is") and infix[1].kind == nnkDotExpr and infix[1][0].isIdent("self"):
          let name = infix[1][1].strVal
          let typ = infix[2]
          ctor.fields.add(Field(name: name, access: Access.Private, typ: typ))
          ctor.body.add(BodySelfAsgn(field: name, rhs: rhs))
    elif n.isBaseCall():
      if not ctor.hasBaseCall:
        ctor.hasBaseCall = true
        ctor.body.add(
          BodyStmtBaseCall(baseCall: n.parseBaseCall())
        )
      else:
        error "Class definition must have only one base call.", n
    else:
      ctor.body.add(BodyStmtNode(node: n))
  # echo ctor.repr


proc parseConstructor(n: NimNode): Constructor =
  expectKinds n, {nnkCommand, nnkCall}
  if n.kind == nnkCommand:
    let name =
      if n[0].kind == nnkCall:
        some(n[0][1].strVal)
      else:
        none(string)
    expectKind n[1], nnkLambda
    let ctorLambda = n[1]
    # Copy children 1..n of ProcTy's FormalParams
    var args = newSeq[NimNode]()
    let formalParams = ctorLambda.formalParams
    for i in 1 ..< formalParams.len:
      args.add(formalParams[i])
    result = Constructor(name: name, args: args) #, fields: parseConstructorBody(ctorLambda.procBody))
    parseConstructorBody(ctorLambda.procBody, result)
  else:
    #let name = n[1].strVal
    #Constructor(name: some(name), args: @[])
    error "not supported"

# -----------------------------------------------------------------------------
# Body parsing
# -----------------------------------------------------------------------------

type
  Func = ref object
    node: NimNode

  Body = ref object
    ctor: Constructor
    funcs: seq[Func]

proc parseBody(body: NimNode): Body =
  result = Body()
  var found = false
  for n in body:
    if n.isConstructor():
      if not found:
        result.ctor = n.parseConstructor()
        found = true
      else:
        error "Class definition must have only one constructor.", n
    elif n.kind == nnkProcDef or n.kind == nnkMethodDef:
      result.funcs.add(Func(node: n))
    else:
      error "Disallowed node in class definition:\n" & n.repr, n

  if not found:
    error "No constructor found."


# -----------------------------------------------------------------------------
# Assembly of output procs
# -----------------------------------------------------------------------------

proc assembleTypeSection(classDef: ClassDef, baseSymbol: NimNode, ctor: Constructor): NimNode =
  # create type fields from exported methods
  let fields =
    if ctor.fields.len == 0:
      newEmptyNode()
    else:
      let reclist = newNimNode(nnkRecList)
      for field in ctor.fields:
        let fieldName =
          if field.access == Access.Public:
            publicIdent(field.name)
          else:
            ident(field.name)
        let fieldType = field.typ
        let fieldDef = newIdentDefs(fieldName, fieldType)
        reclist.add(fieldDef)
      reclist

  # build type section
  let typeSection = newNimNode(nnkTypeSection)
  let typeDef = newNimNode(nnkTypeDef).add(
    publicIdent(classDef.identClass.strVal),
    classDef.genericParams,
    newNimNode(nnkRefTy).add(
      newNimNode(nnkObjectTy).add(
        newEmptyNode(),
        newNimNode(nnkOfInherit).add(
          baseSymbol
        ),
        fields,
      )
    )
  )
  typeSection.add(typeDef)
  return typeSection

proc assemblePatchProc(classDef: ClassDef, baseSymbol: NimNode, ctor: Constructor): NimNode =

  # main proc def
  let selfIdent = ident "self"
  result = newProc(
    publicIdent("patch"),
    [newEmptyNode(), newIdentDefs(selfIdent, classDef.rawClassDef)],
  )
  for arg in ctor.args:
    result.formalParams.add(arg)

  # attach generic params
  if classDef.genericParams.len > 0:
    result.genericParams = newNimNode(nnkGenericParams)
    for genericParam in classDef.genericParams:
      result.genericParams.add(genericParam)

  # 1. ctor body (base call + var defs + init code)
  for bodyStmt in ctor.body:
    matchInstance:
      case bodyStmt:
      of BodyStmtNode:
        result.procBody.add(bodyStmt.node)
      of BodySelfAsgn:
        result.procBody.add(newAssignment(
          newDotExpr(selfIdent, ident(bodyStmt.field)),
          bodyStmt.rhs,
        ))
      of BodyStmtBaseCall:
        let baseCall = bodyStmt.baseCall
        # make base call
        let patchCallBase = newCall(ident "patch")
        patchCallBase.copyLineInfo(baseCall.origNode)
        patchCallBase.add(newCall(baseSymbol, selfIdent))
        for arg in baseCall.args:
          patchCallBase.add(arg)
        result.procBody.add(patchCallBase)

  # echo "patchProc:\n", result.treeRepr


proc assembleConstructorBody(procDef: NimNode, classDef: ClassDef, ctor: Constructor) =
  # construct self
  procDef.procBody.add(
    newVarStmt(
      ident "self",
      newCall(classDef.rawClassDef)
    )
  )

  # call patch
  let patchCall = newCall(
    ident "patch",
  )
  patchCall.add(ident "self")
  for arg in ctor.args:
    patchCall.add(arg[0])
  procDef.procBody.add(patchCall)

  # return expression
  procDef.procBody.add(ident "self")


proc assembleNamedConstructor(name: string, classDef: ClassDef, ctor: Constructor): NimNode =
  result = newProc(publicIdent(name), [], newStmtList())

  # attach formal params
  result.formalParams.add(classDef.rawClassDef) # return type
  for arg in ctor.args:
    result.formalParams.add(arg)

  # attach generic params
  if classDef.genericParams.len > 0:
    result.genericParams = newNimNode(nnkGenericParams)
    for genericParam in classDef.genericParams:
      result.genericParams.add(genericParam)

  assembleConstructorBody(result, classDef, ctor)


proc assembleGenericConstructor(classDef: ClassDef, ctor: Constructor): NimNode =
  result = newProc(publicIdent("init"), [], newStmtList())

  # attach formal params
  result.formalParams.add(classDef.rawClassDef) # return type
  result.formalParams.add(
    newIdentDefs(
      genSym(nskParam, "T"),
      newNimNode(nnkBracketExpr).add(
        ident "typedesc",
        classDef.identClass,
      )
    )
  )
  for arg in ctor.args:
    result.formalParams.add(arg)

  # attach generic params
  if classDef.genericParams.len > 0:
    result.genericParams = newNimNode(nnkGenericParams)
    for genericParam in classDef.genericParams:
      result.genericParams.add(genericParam)

  assembleConstructorBody(result, classDef, ctor)


# -----------------------------------------------------------------------------
# Main class macro impl
# -----------------------------------------------------------------------------

macro classImpl(definition: untyped, base: typed, body: untyped): untyped =

  result = newStmtList()
  echo "-----------------------------------------------------------------------"
  echo definition.treeRepr
  echo body.treeRepr
  echo "-----------------------------------------------------------------------"

  # extract infos from definition
  let classDef = parseDefinition(definition)

  # get base TypeDef
  # echo base.treeRepr
  # echo base.getTypeInst.treeRepr
  expectKind base.getTypeInst, nnkBracketExpr
  expectLen base.getTypeInst, 2
  let baseSymbol = base.getTypeInst[1]  # because its a typedesc, the type symbol is child 1

  # extract blocks and fields
  let body = parseBody(body)
  let ctor = body.ctor

  #[
  if not overloadInfo.isFullyOverloaded and not parsedBody.hasBaseCall:
    error &"Class '{classDef.name}' needs to have a base(...) call because it doesn't overload all parent methods.", body
  if parsedBody.hasBaseCall and baseSymbol.strVal == "RootObj":
    error &"Class '{classDef.name}' cannot have base call, because it is a root class.", body
    ]#

  let typeSection = assembleTypeSection(classDef, baseSymbol, ctor)
  let patchProc = assemblePatchProc(classDef, baseSymbol, ctor)

  result.add(typeSection)
  result.add(patchProc)

  # Add accessor templates
  template accessor(field, fieldType, selfSymbol): untyped {.dirty.} =
    template field*(self: selfSymbol): fieldType = self.field
  for field in ctor.fields:
    if field.access == Access.Readable:
      result.add(getAst(accessor(
        ident(field.name),
        field.typ,
        classDef.identClass),
      ))

  # Add funcs (forward declarations)
  for f in body.funcs:
    let n = f.node.copyNimTree()
    n.formalParams.insert(1, newIdentDefs(ident "self", classDef.identClass))
    n.procBody = newEmptyNode()
    result.add(n)

  # Add funcs
  for f in body.funcs:
    let n = f.node.copyNimTree()
    n.formalParams.insert(1, newIdentDefs(ident "self", classDef.identClass))
    result.add(n)

  # Generate constructors if not abstract
  let genericConstructorProc = assembleGenericConstructor(classDef, ctor)
  result.add(genericConstructorProc)
  for name in ctor.name:
    let namedConstructorProc = assembleNamedConstructor(name, classDef, ctor)
    result.add(namedConstructorProc)

  # Take a copy as a work-around for: https://github.com/nim-lang/Nim/issues/10902
  result = result.copy
  echo result.repr
  # echo result.treeRepr


# -----------------------------------------------------------------------------
# Public macros
# -----------------------------------------------------------------------------

macro class*(definition: untyped, body: untyped): untyped =
  ## Class definition that derives from RootObj.
  if definition.kind == nnkInfix and definition[0].strVal == "of":
    result = newCall(
      bindSym "classImpl",
      definition[1],
      definition[2],
      body,
    )
  else:
    let base = getType(typedesc[RootObj])
    result = newCall(
      bindSym "classImpl",
      definition,
      base,
      body,
    )

# -----------------------------------------------------------------------------
# Dev sandbox
# -----------------------------------------------------------------------------

when false:
  # for quick tree dumps

  macro test1(n: untyped): untyped =
    echo n.treeRepr

  macro test0(n: untyped): untyped =
    discard

  static:
    test0:
      constructor(named) = proc (x: T = 10)
      constructor = proc(x: T = 10)

      # ctor[T](x: T = 10)  # default args not allowed, so we need a ProcTy
      @ctor proc(x: T = 10)
      @ctor(named) proc(x: T = 10)

      ctor proc(x: T = 10)
      ctor(named) proc(x: T = 10)

      base(x, y)

      proc t[T](x: T = 10)

    test0:
      proc patch[T](x: T) =
        discard

    test0:
      proc init(T: typedesc[Foo])

    test1:
      type
        Abstract* = ref object of RootObj
          #id*: proc (): string
          id* {.abstractMethod.}: proc (): string

    error("Tree dumped")
