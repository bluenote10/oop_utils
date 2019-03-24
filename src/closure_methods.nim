import macros
import strformat
import options
import sets

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

iterator items*[T](o: Option[T]): T =
  if o.isSome:
    yield o.get


proc expectKinds*(n: NimNode, kinds: set[NimNodeKind]) {.compileTime.} =
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


proc findBlock(n: NimNode, name: string): NimNode =
  # To be removed after constructor syntax change...
  var found = newSeq[NimNode]()
  for child in n:
    if child.kind == nnkCall and child[0].kind == nnkIdent and child[0].strVal == name:
      found.add(child)
  if found.len == 0:
    error &"Could not find a section of type '{name}'"
  if found.len == 2:
    error &"There are {found.len} sections of type '{name}'; only one section allowed."
  return found[0][1]

# -----------------------------------------------------------------------------
# Constructor parsing
# -----------------------------------------------------------------------------

type
  ParsedConstructor = ref object
    name: Option[string]
    args: seq[NimNode]

proc isConstructor(n: NimNode): bool =
  n.kind == nnkAsgn and (
    (n[0].kind == nnkIdent and n[0].strVal == "constructor") or
    (n[0].kind == nnkCall and n[0][0].kind == nnkIdent and n[0][0].strVal == "constructor")
  )

proc parseConstructor(n: NimNode): ParsedConstructor =
  let name =
    if n.kind == nnkCall:
      some(n[1].strVal)
    else:
      none(string)
  var args = newSeq[NimNode]()
  let formalParams = n[1][0]
  for i in 1 ..< formalParams.len:
    args.add(formalParams[i])
  ParsedConstructor(name: name, args: args)

# -----------------------------------------------------------------------------
# Base call parsing
# -----------------------------------------------------------------------------

type
  ParsedBaseCall = ref object
    args: seq[NimNode]

proc isBaseCall(n: NimNode): bool =
  n.kind == nnkCall and n[0].strVal == "base"

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


proc convertProcDefIntoField(procdef: NimNode): NimNode =
  # We need to turn funcName into funcName* for export
  let procIdent = procdef[0]
  let field = newNimNode(nnkPostfix).add(
    ident "*",
    procIdent,
  )
  let fieldType = newNimNode(nnkProcTy).add(
    procdef[3], # copy formal params
    newEmptyNode(),
  )
  result = newIdentDefs(field, fieldType)


proc parseProcDef(procDef: NimNode): ParsedProc =
  expectKind procDef, nnkProcDef
  if procDef[0].kind == nnkPostfix and procDef[0][0].strVal == "*":
    # get rid of postfix export "*"
    let transformedProcDef = procDef.copyNimTree()
    transformedProcDef[0] = procDef[0][1]
    result = ExportedProc(
      name: transformedProcDef[0].strVal,
      procDef: transformedProcDef,
      isAbstract: false,
      fieldDef: convertProcDefIntoField(transformedProcDef)
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
    ctor: Option[ParsedConstructor]
    baseCall: Option[ParsedBaseCall]
    exportedProcs: seq[ExportedProc]
    privateProcs: seq[PrivateProc]
    varDefs: seq[NimNode]

proc parseBody(body: NimNode): ParsedBody =
  result = ParsedBody(
    ctor: none(ParsedConstructor),
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
        error "Class definition must have only one constructor"
    elif n.isBaseCall():
      if result.baseCall.isNone:
        result.baseCall = some(n.parseBaseCall())
      else:
        error "Class definition must have only one base call"

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
    classDef.identClass,
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

proc assemblePatchProc(constructorDef: NimNode, classDef: ClassDef, baseSymbol: NimNode, parsedBody: ParsedBody): NimNode =
  expectKind constructorDef, nnkProcDef
  # echo "constructorDef:\n", constructorDef.treeRepr

  # Copy formal params of constructor def into the closure result type.
  # Note that we only have to copy from child 1 onwards, because child
  # 0 is the return type, and our function returns nothing
  let ctorFormalParams = newNimNode(nnkFormalParams)
  ctorFormalParams.add(newEmptyNode()) # void return type
  let formalParamsConstructorDef = constructorDef[3]
  for i in 1 ..< formalParamsConstructorDef.len:
    ctorFormalParams.add(formalParamsConstructorDef[i])

  let returnType = newNimNode(nnkProcTy).add(
    ctorFormalParams,
    newEmptyNode(),
  )

  # main proc def
  result = newProc(
    ident "patch",
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

  # 2. var defs
  for varDef in parsedBody.varDefs:
    closure.procBody.add(varDef)

  # 3. private procs
  for privateProc in parsedBody.privateProcs:
    closure.procBody.add(
      privateProc.procDef
    )

  # 4. exported procs
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


proc assembleNamedConstructor(constructorDef: NimNode, classDef: ClassDef, parsedBody: ParsedBody): NimNode =
  expectKind constructorDef, nnkProcDef
  result = constructorDef.copyNimTree()
  result.procBody = newStmtList()

  # We inject the return type to the ctor proc as a convenience.
  # Return type is at: FormalParams at index 3, return type at index 0.
  # Note that we have to use the original definition node, not just
  # the identClass, because we need a BracketExpr in case of generics.
  # For now we make the injection optional, because of a macro bug
  # in Nim that prevents injecting the type with generics.
  if result[3][0].kind == nnkEmpty:
    result[3][0] = classDef.rawClassDef

  # construct self
  result.procBody.add(
    newLetStmt(
      ident "self",
      newCall(classDef.rawClassDef)
    )
  )

  # call constructor impl
  let patchCall = newCall(
    newCall(
      ident "patch",
      ident "self",
    )
  )
  for i in 1 ..< constructorDef.formalParams.len:
    patchCall.add(constructorDef.formalParams[i][0])
  result.procBody.add(patchCall)

  # return expression
  result.procBody.add(ident "self")


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

  # extract blocks and fields
  let parsedBody = parseBody(body)
  let constructorBlock = findBlock(body, "constructor")

  # get base TypeDef
  expectKind base.getTypeInst, nnkBracketExpr
  expectLen base.getTypeInst, 2
  let baseSymbol = base.getTypeInst[1]  # because its a typedesc, the type symbol is child 1

  # recursive extraction of all base methods
  var baseMethods = newSeq[string]()
  extractBaseMethods(baseSymbol, baseMethods)
  let overloadInfo = compareProcs(baseMethods, parsedBody.exportedProcs)

  let constructorDef = constructorBlock[0]  # TODO: find ProcDef, check only 1
  # We inject the return type to the ctor proc as a convenience.
  # Return type is at: FormalParams at index 3, return type at index 0.
  # Note that we have to use the original definition node, not just
  # the identClass, because we need a BracketExpr in case of generics.
  # For now we make the injection optional, because of a macro bug
  # in Nim that prevents injecting the type with generics.
  if constructorDef[3][0].kind == nnkEmpty:
    constructorDef[3][0] = classDef.rawClassDef

  let typeSection = assembleTypeSection(classDef, baseSymbol, overloadInfo.newProcs)
  let namedConstructorProc = assembleNamedConstructor(constructorDef, classDef, parsedBody)
  let patchProc = assemblePatchProc(constructorDef, classDef, baseSymbol, parsedBody)

  result.add(typeSection)
  result.add(patchProc)
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
      #ctor[T](x: T = 10)
      proc t[T](x: T = 10)

    test1:
      proc patch[T](x: T) =
        discard

    error("Tree dumped")
