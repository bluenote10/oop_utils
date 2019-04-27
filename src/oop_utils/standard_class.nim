import macros
import strformat
import options
import sets
import tables
import strutils
import sequtils
import sugar

import private/utils
import private/common
import unwrap

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
    rawBody: NimNode

proc field(name: string, access: Access, rhs: NimNode, typ: NimNode = nil): Field =
  Field(
    name: name,
    access: access,
    typ: typ,
    rhs: rhs,
  )

proc defaultConstructor(name = none(string)): Constructor =
  Constructor(
    name: name,
    args: @[],
    fields: @[],
    body: @[],
    rawBody: newStmtList(),
  )


proc parseSelfBlock(body: NimNode, ctor: Constructor) =
  var baseCall = none(ParsedBaseCall)

  for n in body:
    if n.isIdent:
      ctor.fields.add(field(n.strVal, Access.Private, n))
    elif n.isCommand and n[0].isIdent and n[1].isAccQuoted:
      let field = n[0]
      ctor.fields.add(field(field.strVal, Access.Private, field))
    elif n.isAsgn:
      var access: Access
      var field: NimNode
      if n[0].isIdent:
        field = n[0]
        access = Access.Private
      elif n[0].isCommand and n[0][0].isIdent and n[0][1].isAccQuoted and n[0][1][0].isIdent("*"):
        field = n[0][0]
        access = Access.Public
      elif n[0].isCommand and n[0][0].isIdent and n[0][1].isAccQuoted and n[0][1][0].isIdent("+"):
        field = n[0][0]
        access = Access.Readable
      else:
        error &"Unsupported expression in self block: {n.repr}", n
      let texpr = n[1]
      ctor.fields.add(field(field.strVal, access, texpr))
    elif n.isBaseCall:
      if baseCall.isNone:
        baseCall = some(n.parseBaseCall)
      else:
        error "Class definition must have only one base call.", n
    else:
      error &"Unsupported expression in self block: {n.repr}", n

  ctor.body.add(BodySelfBlock(baseCall: baseCall))


proc parseConstructorBody(ctorBody: NimNode, ctor: Constructor) =
  var foundSelfBlock = false

  for n in ctorBody:
    if n.isCall and n[0].isIdent("self"):
      foundSelfBlock = true
      parseSelfBlock(n[1].assumeStmtList(), ctor)
    else:
      ctor.body.add(BodyStmtNode(node: n))

  # Reached the end of the ctor body without having found a self block?
  # => return a dummy self block so that automatic generation of base
  #    calls works.
  if not foundSelfBlock:
    ctor.body.add(BodySelfBlock(baseCall: none(ParsedBaseCall)))

  # echo ctor.repr


proc parseConstructor(n: NimNode): Option[Constructor] =
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

  if case1:
    let name =
      if n[0].kind == nnkCall:
        some(n[0][1].strVal)
      else:
        none(string)

    # Validate that the ctor has a body. Without a body the AST element
    # becomes nnkProcTy instead of an nnkLambda.
    if n[1].kind == nnkProcTy:
      error "Constructor needs body needs an implementation (or omit the proc() expression entirely).", n

    expectKind n[1], nnkLambda
    let ctorLambda = n[1]

    # Validate that the ctor doesn't have a return type. Otherwise
    # the return type of the pseudo ctor could be wrong.
    if ctorLambda.formalParams[0].kind != nnkEmpty:
      error "Constructor must not have a return type.", n

    # Copy children 1..n of ProcTy's FormalParams
    var args = newSeq[NimNode]()
    let formalParams = ctorLambda.formalParams
    for i in 1 ..< formalParams.len:
      args.add(formalParams[i])

    let ctor = Constructor(name: name, args: args, rawBody: ctorLambda.procBody) #, fields: parseConstructorBody(ctorLambda.procBody))
    parseConstructorBody(ctorLambda.procBody, ctor)
    return some(ctor)

  elif case2:
    expectKind n[1], nnkIdent
    let name = n[1].strVal
    # The len determines whether the ctor call has a body or not.
    if n.len == 2:
      let ctor = defaultConstructor(some(name))
      return some(ctor)
    elif n.len == 3:
      let ctor = defaultConstructor(some(name))
      parseConstructorBody(n[2].assumeStmtList, ctor)
      return some(ctor)
    else:
      error &"Constructor call has unexpected len: {n.repr}"

  else:
    #error "Invalid constructor syntax", n
    return none(Constructor)


type
  AnyProcDef = object

proc unwrapImpl(T: typedesc[Constructor], n: NimNode): Option[Constructor] =
  parseConstructor(n)

proc unwrapImpl(T: typedesc[AnyProcDef], n: NimNode): Option[NimNode] =
  if n.kind == nnkProcDef or n.kind == nnkMethodDef or n.kind == nnkTemplateDef:
    some(n)
  else:
    none(NimNode)

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
    unwrap(n):
      ctor @ Constructor:
        if result.ctor.isNone:
          result.ctor = some(ctor)
        else:
          error "Class definition must have only one constructor.", n
      n @ AnyProcDef:
        result.funcs.add(Func(node: n))
      _:
        error "Disallowed node in class definition:\n" & n.repr, n


# -----------------------------------------------------------------------------
# Assembly of output procs
# -----------------------------------------------------------------------------

proc extractFields(ctor: Constructor, pseudoCtorBlock: NimNode): seq[Field] =

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
    # error &"Could not find self block in {pseudoCtor.repr}", pseudoCtor
    # For pure abstract classes we need to allow ctors without a self
    # block. Can this lead to issues?
    return @[]

  # extract field types from the typed ctor
  var fieldTypes = initTable[string, NimNode]()
  for n in selfBlockContent.assumeStmtList:
    if n.isVariableBinding:
      for identDef in n:
        let field = identDef[0]
        let texpr = identDef[2]
        # echo texpr.treeRepr
        # echo "getType: ", texpr.getType.treeRepr
        # echo "getTypeImpl: ", texpr.getTypeImpl.treeRepr
        # echo "getTypeInst: ", texpr.getTypeInst.treeRepr
        fieldTypes[field.strVal] = toUntyped(texpr.getTypeInst)

  if fieldTypes.len != ctor.fields.len:
    error &"Typed constructor has {fieldTypes.len} fields, but untyped constructor has {ctor.fields.len}"

  # patch types into the untyped ctor fields
  var fields = ctor.fields
  for i in 0 ..< fields.len:
    let name = fields[i].name
    if not fieldTypes.contains(name):
      error &"Field {name} not found in typed constructor"
    let typ = fieldTypes[name]
    fields[i].typ = typ

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


template fallbackBaseCall(patchFunc: untyped, selfIdent: untyped, baseSymbol: untyped, baseName: string) =
  when compiles(patchFunc(baseSymbol(selfIdent))):
    patchFunc(baseSymbol(selfIdent))
  else:
    {.error: "Class needs to have an explicit base call, " &
             "because the constructor of '" & baseName & "' requires parameters.".}


proc assemblePatchProc(classDef: ClassDef, baseSymbol: NimNode, ctor: Constructor, fields: seq[Field]): NimNode =

  # main proc def
  let selfIdent = ident "self"
  result = newProc(
    publicIdent("patch" & classDef.name),
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
          let patchCallBase = newCall(ident "patch" & baseSymbol.strVal)
          patchCallBase.copyLineInfo(baseCall.origNode)
          patchCallBase.add(newCall(baseSymbol, selfIdent))
          for arg in baseCall.args:
            patchCallBase.add(arg)
          result.procBody.add(patchCallBase)
        if bodyStmt.baseCall.isNone:
          if baseSymbol.strVal != "RootObj":
            let patchFunc = ident "patch" & baseSymbol.strVal
            #result.procBody.add(getAst(fallbackBaseCall(selfIdent, baseSymbol, baseSymbol.strVal)))
            result.procBody.add(newCall(bindsym "fallbackBaseCall",
              [patchFunc, selfIdent, baseSymbol, newStrLitNode(baseSymbol.strVal)]
            ))
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
    ident "patch" & classDef.name,
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
  let ctor = body.ctor.get(defaultConstructor())
  let fields = extractFields(ctor, pseudoCtor)

  #[
  # TODO: add verification of base call usage
  if not overloadInfo.isFullyOverloaded and not parsedBody.hasBaseCall:
    error &"Class '{classDef.name}' needs to have a base(...) call because it doesn't overload all parent methods.", body
  if parsedBody.hasBaseCall and baseSymbol.strVal == "RootObj":
    error &"Class '{classDef.name}' cannot have base call, because it is a root class.", body
  ]#

  let typeSection = assembleTypeSection(classDef, baseSymbol, fields)
  result.add(typeSection)

  # Generate accessor templates
  template accessor(field, fieldType, selfSymbol): untyped {.dirty.} =
    template field*(self: selfSymbol): fieldType = self.field
  for field in fields:
    if field.access == Access.Readable:
      result.add(getAst(accessor(
        ident(field.name),
        field.typ,
        classDef.identClass),
      ))

  # Generate funcs forward declarations
  for f in body.funcs:
    if f.node.kind != nnkTemplateDef: # templates cannot be forward declared
      let n = f.node.copyNimTree()
      n.formalParams.insert(1, newIdentDefs(ident "self", classDef.identClass))
      n.procBody = newEmptyNode()
      result.add(n)

  # Generate funcs
  for f in body.funcs:
    let n = f.node.copyNimTree()
    n.formalParams.insert(1, newIdentDefs(ident "self", classDef.identClass))
    if n.kind == nnkMethodDef and n.procBody.kind == nnkEmpty:
      n.procBody = generateUnimplementedBody(n.procName)
    result.add(n)

  # Generate patch proc
  let patchProc = assemblePatchProc(classDef, baseSymbol, ctor, fields)
  result.add(patchProc)

  # Generate constructors
  # TODO: Because of the fallback logic to the default constructor, we currently
  # generate an init proc also for pure abstract classes. The alternative would
  # be to generate ctors only if body.ctor is defined, but than we wouldn't be
  # able to construct non-abstract classes that legitimately omit the ctor proc.
  # A nice addition would be to add check for pure abstract classes.
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
template markReadable*() {.pragma.}

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

#[
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
]#

proc generateTypedCtorBody(ctor: Constructor): NimNode =
  result = newStmtList()
  for statement in ctor.body:
    matchInstance:
      case statement:
      of BodySelfBlock:
        break
      of BodyStmtNode:
        result.add(statement.node)

  let blck = newBlockStmt(ident "self", newStmtList())
  for field in ctor.fields:
    let pragmas = case field.access
      of Access.Private: newSeq[NimNode]()
      of Access.Public: @[ident "markPublic"]
      of Access.Readable: @[ident "markReadable"]
    blck[1].add(newLetStmtWithPragma(ident field.name, field.rhs, pragmas))

  result.add(blck)


proc extractPseudoCtor(body: NimNode): NimNode =
  let ctor = newProc(ident "dummy")
  ctor.pragmas = newNimNode(nnkPragma).add(ident "used")
  for n in body:
    unwrap(n):
      ctorParsed @ Constructor:
        # copy args
        ctor.formalParams = newNimNode(nnkFormalParams).add(newEmptyNode())
        for arg in ctorParsed.args:
          ctor.formalParams.add(arg)
        # transform body
        ctor.procBody = generateTypedCtorBody(ctorParsed) # transformCtorBody(ctorParsed.rawBody)

  result = newBlockStmt(ctor)
  echo result.repr

# -----------------------------------------------------------------------------
# Public macros
# -----------------------------------------------------------------------------

macro class*(definition: untyped, body: untyped): untyped =
  ## Class definition that derives from RootObj.
  let pseudoCtor = extractPseudoCtor(body) # need to copy here in case of "environment misses"?

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
