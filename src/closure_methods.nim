import macros
import strformat
import options
import sets
import strutils

# -----------------------------------------------------------------------------
# Misc publics
# -----------------------------------------------------------------------------

# Both doesn't work...
# template abstractMethod* {.pragma.}
# {.pragma: abstractMethod.}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

iterator items[T](o: Option[T]): T =
  if o.isSome:
    yield o.get


proc expectKinds(n: NimNode, kinds: set[NimNodeKind]) {.compileTime.} =
  ## checks that `n` is of kind `k`. If this is not the case,
  ## compilation aborts with an error message. This is useful for writing
  ## macros that check the AST that is passed to them.
  if not kinds.contains(n.kind): error("Expected a node of kinds " & $kinds & ", got " & $n.kind, n)

proc procBody(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkLambda}
  n[n.len - 1]

proc `procBody=`(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkLambda}
  expectKind other, nnkStmtList
  n[n.len - 1] = other

proc formalParams(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkLambda}
  n[3]

proc `formalParams=`(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkLambda}
  expectKind other, nnkFormalParams
  n[3] = other

proc genericParams(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef}
  n[2]

proc `genericParams=`(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkLambda}
  expectKind other, nnkGenericParams
  n[2] = other

proc publicIdent(s: string): NimNode =
  newNimNode(nnkPostfix).add(
    ident "*",
    ident s,
  )

# -----------------------------------------------------------------------------
# Base type inspection
# -----------------------------------------------------------------------------

proc extractBaseMethods(baseSymbol: NimNode, baseMethods: var seq[string]) =
  ## Recursively traverses over base types and collects their method fields.
  ## TODO: collect 'abstract' custom annotation field information.

  let baseTypeDef = baseSymbol.getImpl
  let baseObjectTy = baseTypeDef[2][0]
  # echo baseTypeDef.treeRepr

  # inheritance is at ObjectTy index 1 -- recurse over parents
  if baseObjectTy.len >= 1 and baseObjectTy[1].kind == nnkOfInherit:
    let baseBaseSymbol = baseObjectTy[1][0]
    extractBaseMethods(baseBaseSymbol, baseMethods)

  # reclist is at ObjectTy index 2
  let baseRecList = if baseObjectTy.len >= 3: baseObjectTy[2] else: newEmptyNode()
  for identDef in baseRecList:
    if identDef.kind == nnkIdentDefs:
      let nameNode = identDef[0]
      let typeNode = identDef[1]
      if nameNode.kind == nnkPostfix and nameNode.len == 2: # because of export * symbol
        baseMethods.add(nameNode[1].strVal)
      elif nameNode.kind == nnkPragmaExpr and nameNode.len == 2 and
           nameNode[0].kind == nnkPostfix and nameNode[0].len == 2:
        # TODO: extract isAbstract: true
        baseMethods.add(nameNode[0][1].strVal)
      else:
        error &"Unexpected node in base rec list:{nameNode.repr}"
    else:
      error &"Expected nnkIdentDefs, got {identDef.repr}"

# -----------------------------------------------------------------------------
# Class name parsing (mainly needed to split generic params)
# -----------------------------------------------------------------------------

type
  ClassDef = ref object
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
    error &"Cannot parse class definition: {n.repr}"


proc parseDefinition(n: NimNode): ClassDef =
  result = ClassDef()
  if n.kind == nnkInfix and n[0].strVal == "of":
    result.rawClassDef = n[1]
    result.parseClassName(n[1])
    #result.identBaseClass = n[2]
    error "of syntax is deprecated"
  else:
    result.rawClassDef = n
    result.parseClassName(n)
    #result.identBaseClass = ident "RootObj"

# -----------------------------------------------------------------------------
# Constructor parsing
# -----------------------------------------------------------------------------

type
  Constructor = ref object
    name: Option[string]
    args: seq[NimNode]

proc isConstructor(n: NimNode): bool =
  echo n.treeRepr
  # case1 matches:
  # - ctor proc(...)
  # - ctor(named) proc(...)
  let case1 = n.kind == nnkCommand and (
    (n[0].kind == nnkIdent and n[0].strVal == "ctor") or
    (n[0].kind == nnkCall and n[0][0].kind == nnkIdent and n[0][0].strVal == "ctor")
  )
  # case2 matches ctor without arguments:
  # - ctor(named)
  let case2 = n.kind == nnkCall and n[0].strVal == "ctor"
  return case1 or case2

proc parseConstructor(n: NimNode): Constructor =
  expectKinds n, {nnkCommand, nnkCall}
  if n.kind == nnkCommand:
    let name =
      if n[0].kind == nnkCall:
        some(n[0][1].strVal)
      else:
        none(string)
    # Copy children 1..n of ProcTy's FormalParams
    var args = newSeq[NimNode]()
    let formalParams = n[1][0]
    for i in 1 ..< formalParams.len:
      args.add(formalParams[i])
    Constructor(name: name, args: args)
  else:
    let name = n[1].strVal
    Constructor(name: some(name), args: @[])

# -----------------------------------------------------------------------------
# Base call parsing
# -----------------------------------------------------------------------------

type
  ParsedBaseCall = ref object
    args: seq[NimNode]

proc isBaseCall(n: NimNode): bool =
  n.kind == nnkCall and n[0].kind == nnkIdent and n[0].strVal == "base"

proc parseBaseCall(n: NimNode): ParsedBaseCall =
  var args = newSeq[NimNode]()
  for i in 1 ..< n.len:
    args.add(n[i])
  ParsedBaseCall(args: args)

# -----------------------------------------------------------------------------
# Proc parsing
# -----------------------------------------------------------------------------

type
  ParsedProc = ref object of RootObj

  ExportedProc = ref object of ParsedProc
    name: string
    procDef: NimNode
    isAbstract: bool
    fieldDef: NimNode

  PrivateProc = ref object of ParsedProc
    name: string
    procDef: NimNode


proc convertProcDefIntoField(procdef: NimNode, isAbstract: bool): NimNode =
  # We need to turn funcName into funcName* for export
  let procIdent = procdef[0]
  let field =
    if not isAbstract: # custom pragma doesn't seem to work, abuse 'used'?
      publicIdent(procIdent.strVal)
    else:
      newNimNode(nnkPragmaExpr).add(
        publicIdent(procIdent.strVal),
        newNimNode(nnkPragma).add(ident "used"),
      )
  let fieldType = newNimNode(nnkProcTy).add(
    procdef[3], # copy formal params
    newEmptyNode(),
  )
  result = newIdentDefs(field, fieldType)


proc parseProcDef(procDef: NimNode): ParsedProc =
  expectKind procDef, nnkProcDef
  if procDef[0].kind == nnkPostfix and procDef[0][0].strVal == "*":
    let name = procDef[0][1].strVal
    let isAbstract = procDef.body.kind == nnkEmpty
    let transformedProcDef = procDef.copyNimTree()

    # transform 1: get rid of postfix export "*"
    transformedProcDef[0] = procDef[0][1]

    # transform 2: inject dummy body for abstracts
    if isAbstract:
      let errorMsg = &"called abstract method '{name}'"
      transformedProcDef.body = newStmtList(
        newCall(ident "doAssert", ident "false", newStrLitNode(errorMsg))
      )

    result = ExportedProc(
      name: name,
      procDef: transformedProcDef,
      isAbstract: isAbstract,
      fieldDef: convertProcDefIntoField(transformedProcDef, isAbstract)
    )
  else:
    result = PrivateProc(
      name: procDef[0].strVal,
      procDef: procDef.copyNimTree(),
    )

# -----------------------------------------------------------------------------
# Body parsing
# -----------------------------------------------------------------------------

type
  ParsedBody = ref object
    ctor: Option[Constructor]
    baseCall: Option[ParsedBaseCall]
    exportedProcs: seq[ExportedProc]
    privateProcs: seq[PrivateProc]
    varDefs: seq[NimNode]

proc parseBody(body: NimNode): ParsedBody =
  result = ParsedBody(
    ctor: none(Constructor),
  )
  for n in body:
    if {nnkVarSection, nnkLetSection, nnkConstSection}.contains(n.kind):
      result.varDefs.add(n.copyNimTree())
    elif n.kind == nnkProcDef:
      let parsedProc = parseProcDef(n)
      if parsedProc of ExportedProc:
        result.exportedProcs.add(parsedProc.ExportedProc)
      elif parsedProc of PrivateProc:
        result.privateProcs.add(parsedProc.PrivateProc)
    elif n.isConstructor():
      if result.ctor.isNone:
        result.ctor = some(n.parseConstructor())
      else:
        error "Class definition must have only one constructor", n
    elif n.isBaseCall():
      if result.baseCall.isNone:
        result.baseCall = some(n.parseBaseCall())
      else:
        error "Class definition must have only one base call", n
    else:
      error "Disallowed node in class definition:\n" & n.repr, n

# -----------------------------------------------------------------------------
# Assembly of output procs
# -----------------------------------------------------------------------------

type
  OverloadInfo = ref object
    isFullyOverloaded: bool
    newProcs: seq[ExportedProc]
    overloadedProcs: seq[ExportedProc]

proc compareProcs(baseProcs: seq[string], exportedProcs: seq[ExportedProc]): OverloadInfo =
  result = OverloadInfo()
  let baseProcsSet = baseProcs.toSet()
  for exportedProc in exportedProcs:
    if baseProcsSet.contains(exportedProc.name):
      result.overloadedProcs.add(exportedProc)
    else:
      result.newProcs.add(exportedProc)

proc assembleTypeSection(classDef: ClassDef, baseSymbol: NimNode, newProcs: seq[ExportedProc]): NimNode =
  # create type fields from exported methods
  let fields =
    if newProcs.len == 0:
      newEmptyNode()
    else:
      let reclist = newNimNode(nnkRecList)
      for exportedProc in newProcs:
        reclist.add(exportedProc.fieldDef)
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


proc newLambda(): NimNode =
  newNimNode(nnkLambda).add(
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode(),
    newNimNode(nnkFormalParams),
    newEmptyNode(),
    newEmptyNode(),
    newStmtList(),
  )

proc convertProcDefIntoLambda(n: NimNode): NimNode =
  result = newLambda()
  result[3] = n[3]
  result[result.len - 1] = n[n.len - 1]
  # echo result.treeRepr

proc assemblePatchProc(classDef: ClassDef, ctor: Constructor, baseSymbol: NimNode, baseMethods: seq[string], parsedBody: ParsedBody): NimNode =

  # Copy formal params of constructor def into the closure result type.
  # Note: Closure has void return type, so add empty first child.
  let ctorFormalParams = newNimNode(nnkFormalParams)
  ctorFormalParams.add(newEmptyNode()) # void return type
  for arg in ctor.args:
    ctorFormalParams.add(arg)   # TODO: maybe strip default arguments

  let returnType = newNimNode(nnkProcTy).add(
    ctorFormalParams,
    newEmptyNode(),
  )

  # main proc def
  result = newProc(
    publicIdent("patch"),
    [returnType, newIdentDefs(ident "self", classDef.rawClassDef)],
  )

  # attach generic params
  result.genericParams = newNimNode(nnkGenericParams)
  for genericParam in classDef.genericParams:
    result.genericParams.add(genericParam)

  # build closure body
  let closure = newLambda()
  closure[3] = ctorFormalParams

  # 1. parent constructor impl call
  for baseCall in parsedBody.baseCall:
    let patchCallBase = newCall(
      newCall(
        ident "patch",
        newCall(baseSymbol, ident "self"),
      )
    )
    for arg in baseCall.args:
      patchCallBase.add(arg)
    closure.procBody.add(patchCallBase)

  # 2. inject base
  if parsedBody.baseCall.isSome:
    closure.procBody.add(
      newVarStmt(ident "base", newNimNode(nnkObjConstr).add(baseSymbol))
    )
    for baseMethod in baseMethods:
      closure.procBody.add(
        newAssignment(
          newDotExpr(ident "base", ident baseMethod),
          newDotExpr(ident "self", ident baseMethod),
        )
      )

  # 3. var defs
  for varDef in parsedBody.varDefs:
    closure.procBody.add(varDef)

  # 4. private procs
  for privateProc in parsedBody.privateProcs:
    closure.procBody.add(
      privateProc.procDef
    )

  # 5. exported procs
  for exportedProc in parsedBody.exportedProcs:
    closure.procBody.add(
      newAssignment(
        newDotExpr(ident "self", ident exportedProc.name),
        convertProcDefIntoLambda(exportedProc.procDef),
      )
    )

  let procBody = newStmtList()
  procBody.add(
    newAssignment(
      ident "result",
      closure,
    )
  )

  # attach proc body
  result.procBody = procBody
  # echo "patchProc:\n", result.treeRepr

proc assembleNamedConstructorBody(procDef: NimNode, classDef: ClassDef, ctor: Constructor) =
  # construct self
  procDef.procBody.add(
    newLetStmt(
      ident "self",
      newCall(classDef.rawClassDef)
    )
  )

  # call patch
  let patchCall = newCall(
    newCall(
      ident "patch",
      ident "self",
    )
  )
  for arg in ctor.args:
    patchCall.add(arg[0])
  procDef.procBody.add(patchCall)

  # return expression
  procDef.procBody.add(ident "self")


proc assembleNamedConstructor(name: string, classDef: ClassDef, ctor: Constructor, parsedBody: ParsedBody): NimNode =
  result = newProc(publicIdent(name), [], newStmtList())

  # Formal params
  result.formalParams.add(classDef.rawClassDef) # return type
  for arg in ctor.args:
    result.formalParams.add(arg)

  assembleNamedConstructorBody(result, classDef, ctor)


proc assembleGenericConstructor(classDef: ClassDef, ctor: Constructor, parsedBody: ParsedBody): NimNode =
  result = newProc(publicIdent("init"), [], newStmtList())

  # Formal params
  result.formalParams.add(classDef.rawClassDef) # return type
  result.formalParams.add(
    newIdentDefs(
      ident "T",
      newNimNode(nnkBracketExpr).add(
        ident "typedesc",
        classDef.identClass,
      )
    )
  )
  for arg in ctor.args:
    result.formalParams.add(arg)

  assembleNamedConstructorBody(result, classDef, ctor)


# -----------------------------------------------------------------------------
# Preprocessing
# -----------------------------------------------------------------------------

proc preprocessBody(body: NimNode) =
  ## Input body preprocessing. Currently used to inject getter/setter.
  var i = 0
  while i < body.len:
    let n = body[i]
    if n.kind == nnkCall and n[0].kind == nnkBracketExpr and n.len >= 2:
      let ident = n[1]
      let callName = n[0][0].strVal
      let typ = n[0][1]

      var produceGetter = false
      var produceSetter = false
      var getterName: string
      var setterName: string
      # make comparisons style insensitive?
      if callName == "getter":
        produceGetter = true
        if n.len == 3:
          getterName = n[2].strVal
        else:
          getterName = "get" & ident.strVal.capitalizeAscii()
      elif callName == "setter":
        produceSetter = true
        if n.len == 3:
          setterName = n[2].strVal
        else:
          setterName = "set" & ident.strVal.capitalizeAscii()
      elif callName == "getterSetter":
        produceGetter = true
        produceSetter = true
        if n.len == 4:
          getterName = n[2].strVal
          setterName = n[3].strVal
        else:
          getterName = "get" & ident.strVal.capitalizeAscii()
          setterName = "set" & ident.strVal.capitalizeAscii()

      echo i
      if produceGetter or produceSetter:
        body.del(i)

      if produceGetter:
        body.insert(i, newProc(
          newNimNode(nnkPostfix).add(
            ident "*",
            ident getterName,
          ),
          [typ],
          newStmtList().add(ident),
        ))
      if produceSetter:
        let paramSym = genSym(nskParam)
        body.insert(i, newProc(
          newNimNode(nnkPostfix).add(
            ident "*",
            ident setterName,
          ),
          [newEmptyNode(), newIdentDefs(paramSym, typ)],
          newStmtList().add(
            newAssignment(ident, paramSym),
          ),
        ))

      if produceGetter and produceSetter:
        i += 1
    i += 1

# -----------------------------------------------------------------------------
# Main class macro impl
# -----------------------------------------------------------------------------

proc classImpl(definition, base, body: NimNode): NimNode =

  result = newStmtList()
  echo "-----------------------------------------------------------------------"
  echo definition.treeRepr
  echo body.treeRepr
  echo "-----------------------------------------------------------------------"

  # extract infos from definition
  let classDef = parseDefinition(definition)

  # get base TypeDef
  echo base.treeRepr
  echo base.getTypeInst.treeRepr
  expectKind base.getTypeInst, nnkBracketExpr
  expectLen base.getTypeInst, 2
  let baseSymbol = base.getTypeInst[1]  # because its a typedesc, the type symbol is child 1

  # extract blocks and fields
  body.preprocessBody()
  let parsedBody = parseBody(body)

  # TODO: add post parse verifications here

  # TODO: allow generation of default ctor only if isAbstract == false
  let ctor = parsedBody.ctor.get(Constructor(name: none(string), args: @[]))

  # recursive extraction of all base methods
  var baseMethods = newSeq[string]()
  extractBaseMethods(baseSymbol, baseMethods)
  let overloadInfo = compareProcs(baseMethods, parsedBody.exportedProcs)

  let typeSection = assembleTypeSection(classDef, baseSymbol, overloadInfo.newProcs)
  let patchProc = assemblePatchProc(classDef, ctor, baseSymbol, baseMethods, parsedBody)

  result.add(typeSection)
  result.add(patchProc)

  # if not isAbstract:
  let genericConstructorProc = assembleGenericConstructor(classDef, ctor, parsedBody)
  result.add(genericConstructorProc)
  for name in ctor.name:
    let namedConstructorProc = assembleNamedConstructor(name, classDef, ctor, parsedBody)
    result.add(namedConstructorProc)

  echo result.repr
  #echo result.treeRepr


# -----------------------------------------------------------------------------
# Public macros
# -----------------------------------------------------------------------------

macro class*(definition: untyped, body: untyped): untyped =
  ## Class definition that derives from RootObj.
  let base = getType(typedesc[RootObj])
  result = classImpl(definition, base, body)

macro classOf*(definition: untyped, base: typed, body: untyped): untyped =
  ## Class defintio that allows to specify a base class.
  result = classImpl(definition, base, body)

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
