import macros
import strformat
import options
import sets
import strutils
import sequtils
import sugar

import private/utils
import private/common

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
  BodySelfBlock = ref object of BodyStmt
    baseCall: Option[ParsedBaseCall]


type
  Access = enum
    Public, Readable, Private

  Field = ref object
    name: string
    access: Access
    typ: NimNode
    rhs: NimNode

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
    (n[0].isIdent("ctor")) or
    (n[0].kind == nnkCall and n[0][0].isIdent("ctor"))
  )
  # case2 matches ctor without arguments:
  # - ctor(named)
  let case2 = n.kind == nnkCall and n[0].isIdent("ctor")
  # Discussion: Do standard_classes need the empty constructor?
  # On first glance: If a class has no fields, there is no need to have a
  # patch functions. But: If there is an inheritance chain of A => B => C
  # and class A has fields, but class B has no (own) fields, it would still
  # be necessary for C to call the empty base() on B, which in turn does
  # the non-empty base(...) call on A.
  # But still: Is a NAMED constructor isn't required, or can the generic
  # patch/init functions serve this purpose?
  return case1 or case2

proc parseConstructorBody(ctorBody: NimNode, ctor: Constructor) =
  for n in ctorBody:
    if n.isCall and n[0].isIdent("self"):
      var baseCall = none(ParsedBaseCall)
      for sub in n[1].assumeStmtList:
        if sub.isBasecall():
          if baseCall.isNone:
            baseCall = some(sub.parseBaseCall)
          else:
            error "Class definition must have only one base call.", sub
      ctor.body.add(BodySelfBlock(baseCall: baseCall))
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
    error "Empty constructors are not yet supported", n

# -----------------------------------------------------------------------------
# Body parsing
# -----------------------------------------------------------------------------

type
  Func = ref object
    node: NimNode

  Body = ref object
    ctor: Option[Constructor]
    funcs: seq[Func]

proc parseBody(body: NimNode): Body =
  result = Body()
  for n in body:
    if n.isConstructor():
      if result.ctor.isNone:
        result.ctor = some(n.parseConstructor())
      else:
        error "Class definition must have only one constructor.", n
    elif n.kind == nnkProcDef or n.kind == nnkMethodDef or n.kind == nnkTemplateDef:
      result.funcs.add(Func(node: n))
    else:
      error "Disallowed node in class definition:\n" & n.repr, n


# -----------------------------------------------------------------------------
# Assembly of output procs
# -----------------------------------------------------------------------------

proc extractFields(pseudoCtorBlock: NimNode): seq[Field] =

  # get procdef
  expectKind pseudoCtorBlock, nnkBlockStmt
  let pseudoCtor = pseudoCtorBlock[1]
  expectKind pseudoCtor, nnkProcDef

  # get self block
  var selfBlockContent: NimNode
  proc isSelfBlock(n: NimNode): bool =
    n.kind == nnkBlockStmt and n[0].strVal == "self"
  for n in pseudoCtor.procBody.assumeStmtList:
    if n.isSelfBlock:
      selfBlockContent = n[1]
  if selfBlockContent.isNil:
    error &"Could not find self block in {pseudoCtor.repr}", pseudoCtor

  # extract fields
  var fields = newSeq[Field]()
  for n in selfBlockContent.assumeStmtList:
    if n.isVariableBinding:
      for identDef in n:
        let field = identDef[0]
        let texpr = identDef[2]
        fields.add(Field(
          name: field.strVal,
          access: Access.Private,
          typ: texpr.getType,
          rhs: toUntyped(texpr), # needed to get rid of already bound symbols
        ))

  return fields


proc assembleTypeSection(classDef: ClassDef, baseSymbol: NimNode, fields: seq[Field]): NimNode =

  # create type fields from exported methods
  let fieldsNode =
    if fields.len == 0:
      newEmptyNode()
    else:
      let reclist = newNimNode(nnkRecList)
      for field in fields:
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
        fieldsNode,
      )
    )
  )
  typeSection.add(typeDef)
  return typeSection


proc assemblePatchProc(classDef: ClassDef, baseSymbol: NimNode, ctor: Constructor, fields: seq[Field]): NimNode =

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
      of BodySelfBlock:
        # base call
        for baseCall in bodyStmt.baseCall:
          let patchCallBase = newCall(ident "patch")
          patchCallBase.copyLineInfo(baseCall.origNode)
          patchCallBase.add(newCall(baseSymbol, selfIdent))
          for arg in baseCall.args:
            patchCallBase.add(arg)
          result.procBody.add(patchCallBase)
        # inits fields
        for field in fields:
          result.procBody.add(newAssignment(
            newDotExpr(selfIdent, ident(field.name)),
            field.rhs,
          ))
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

macro classImpl(definition: untyped, base: typed, pseudoCtor: typed, body: untyped): untyped =

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
  let fields = extractFields(pseudoCtor)

  #[
  # TODO: add verification of base call usage
  if not overloadInfo.isFullyOverloaded and not parsedBody.hasBaseCall:
    error &"Class '{classDef.name}' needs to have a base(...) call because it doesn't overload all parent methods.", body
  if parsedBody.hasBaseCall and baseSymbol.strVal == "RootObj":
    error &"Class '{classDef.name}' cannot have base call, because it is a root class.", body
  ]#

  let typeSection = assembleTypeSection(classDef, baseSymbol, fields)
  result.add(typeSection)

  # Add accessor templates
  template accessor(field, fieldType, selfSymbol): untyped {.dirty.} =
    template field*(self: selfSymbol): fieldType = self.field
  for field in fields:
    if field.access == Access.Readable:
      result.add(getAst(accessor(
        ident(field.name),
        field.typ,
        classDef.identClass),
      ))

  # Add funcs (forward declarations)
  for f in body.funcs:
    if f.node.kind != nnkTemplateDef: # templates cannot be forward declared
      let n = f.node.copyNimTree()
      n.formalParams.insert(1, newIdentDefs(ident "self", classDef.identClass))
      n.procBody = newEmptyNode()
      result.add(n)

  # Add funcs
  for f in body.funcs:
    let n = f.node.copyNimTree()
    n.formalParams.insert(1, newIdentDefs(ident "self", classDef.identClass))
    if n.kind == nnkMethodDef and n.procBody.kind == nnkEmpty:
      n.procBody = generateUnimplementedBody(n.procName)
    result.add(n)

  # Add patch proc
  for ctor in body.ctor:
    let patchProc = assemblePatchProc(classDef, baseSymbol, ctor, fields)
    result.add(patchProc)

  # Generate constructors if not abstract
  for ctor in body.ctor:
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
# Extraction of pseudo ctor
# -----------------------------------------------------------------------------

template markPublic*() {.pragma.}

proc newLetStmtWithPragma*(name, value: NimNode, pragmas: openarray[NimNode] = []): NimNode {.compiletime.} =
  result = newNimNode(nnkLetSection).add(
    newNimNode(nnkIdentDefs).add(
      newNimNode(nnkPragmaExpr).add(
        name,
        newNimNode(nnkPragma).add(pragmas).add(ident "used")
      ),
      newEmptyNode(),
      value
    )
  )

proc transformSelfBlock(body: NimNode): NimNode =
  result = newBlockStmt(ident "self", newStmtList())
  for n in body:
    # echo n.treeRepr
    if n.isIdent:
      result[1].add(newLetStmtWithPragma(n, n))
    elif n.isCommand and n[0].isIdent and n[1].isAccQuoted:
      let field = n[0]
      result[1].add(newLetStmtWithPragma(field, field, [ident "markPublic"]))
    elif n.isAsgn:
      var pragma: seq[NimNode]
      var field: NimNode
      if n[0].isIdent:
        field = n[0]
        pragma = @[]
      elif n[0].isCommand and n[0][0].isIdent and n[0][1].isAccQuoted:
        field = n[0][0]
        pragma = @[ident "markPublic"]
      else:
        error &"Unsupported expression in self block: {n.repr}", n
      let texpr = n[1]
      result[1].add(newLetStmtWithPragma(field, texpr, pragma))
    elif n.isBaseCall:
      discard
    else:
      error &"Unsupported expression in self block: {n.repr}", n

proc transformCtorBody(body: NimNode): NimNode =
  result = newStmtList()
  for n in body:
    if n.isCall and n[0].isIdent("self") and n[1].isStmtList:
      result.add(transformSelfBlock(n[1]))
      return
    else:
      result.add(n)

proc extractPseudoCtor(body: NimNode): NimNode =
  let ctor = newProc(ident "dummy")
  for n in body:
    if n.isConstructor():
      let ctorLambda = n[1]
      expectKind ctorLambda, nnkLambda
      ctor.formalParams = ctorLambda.formalParams
      ctor.procBody = transformCtorBody(ctorLambda.procBody)

      # Validate that the ctor doesn't have a return type. Otherwise
      # the return type of the pseudo ctor could be wrong.
      if ctor.formalParams[0].kind != nnkEmpty:
        error "Constructor must not have a return type.", n

  result = newBlockStmt(ctor)
  echo result.repr

# -----------------------------------------------------------------------------
# Public macros
# -----------------------------------------------------------------------------

macro class*(definition: untyped, body: untyped): untyped =
  ## Class definition that derives from RootObj.
  let pseudoCtor = extractPseudoCtor(body.copyNimTree()) # without copying there is an "environment misses" error...

  let (identDef, identBase) =
    if definition.kind == nnkInfix and definition[0].strVal == "of":
      (definition[1], definition[2])
    else:
      (definition, getType(typedesc[RootObj]))

  result = newCall(
    bindSym "classImpl",
    identDef,
    identBase,
    pseudoCtor,
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
