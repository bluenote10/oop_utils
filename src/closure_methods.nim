import macros
import strformat
import options
import sets
import strutils
import sequtils

import closure_methods/match_instance

export match_instance

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

template implementInfo(abstracts: seq[string], implements: seq[string]) {.pragma.}

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

proc genericParams(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef}
  n[2]

proc `genericParams=`(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkLambda}
  expectKind other, nnkGenericParams
  n[2] = other

proc formalParams(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkLambda}
  n[3]

proc `formalParams=`(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkLambda}
  expectKind other, nnkFormalParams
  n[3] = other

proc pragmas(n: NimNode): NimNode =
  expectKinds n, {nnkProcDef, nnkLambda}
  n[4]

proc `pragmas=`(n: NimNode, other: NimNode) =
  expectKinds n, {nnkProcDef, nnkLambda}
  expectKind other, nnkPragma
  n[4] = other

proc publicIdent(s: string): NimNode =
  newNimNode(nnkPostfix).add(
    ident "*",
    ident s,
  )

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
  result.formalParams = n.formalParams
  result.procBody = n.procBody

# -----------------------------------------------------------------------------
# Base type inspection
# -----------------------------------------------------------------------------

type
  BaseAnnotation = ref object
    abstracts: seq[string]
    implements: seq[string]

  BaseMethods = ref object
    methodsAbstract: HashSet[string]
    methodsImplemented: HashSet[string]

iterator allMethods(baseMethods: BaseMethods): string =
  for p in baseMethods.methodsAbstract:
    yield p
  for p in baseMethods.methodsImplemented:
    yield p

proc extractBaseAnnotation(n: NimNode): BaseAnnotation =
  ## Helper functions that extracts all the abstract/implemented
  ## methods introduced by a type.
  # echo n.treeRepr
  expectKind n, nnkTypeDef
  expectKind n[0], nnkPragmaExpr
  expectKind n[0][1], nnkPragma
  expectKind n[0][1][0], nnkCall
  expectKind n[0][1][0][0], nnkSym # the implementInfo symbol
  let implementInfoNode = n[0][1][0]
  let abstractsNode = implementInfoNode[1]
  let implementsNode = implementInfoNode[2]
  expectKind abstractsNode[0], nnkSym # the @ symbol
  expectKind abstractsNode[1], nnkBracket
  expectKind implementsNode[0], nnkSym # the @ symbol
  expectKind implementsNode[1], nnkBracket
  result = BaseAnnotation()
  for strLit in abstractsNode[1]:
    expectKind strLit, nnkStrLit
    result.abstracts.add(strLit.strVal)
  for strLit in implementsNode[1]:
    expectKind strLit, nnkStrLit
    result.implements.add(strLit.strVal)


proc extractBaseMethods(baseSymbol: NimNode, baseMethods: var BaseMethods) =
  ## Recursively traverses over base types and collects their method fields.
  let baseTypeDef = baseSymbol.getImpl
  let baseObjectTy = baseTypeDef[2][0]
  # echo baseTypeDef.treeRepr
  # echo baseObjectTy.treeRepr

  # echo baseSymbol.getImpl.treeRepr
  # echo baseSymbol.getTypeImpl.treeRepr
  # echo baseSymbol.getTypeInst.treeRepr
  # echo baseSymbol.getCustomPragmaVal("implementInfo")
  # echo baseSymbol.getTypeInst.getCustomPragmaVal("implementInfo")

  # inheritance is at ObjectTy index 1 -- recurse over parent first
  if baseObjectTy.len >= 1 and baseObjectTy[1].kind == nnkOfInherit:
    let baseBaseSymbol = baseObjectTy[1][0]
    extractBaseMethods(baseBaseSymbol, baseMethods)

  let baseAnnotation =
    if baseSymbol.strVal == "RootObj":
      BaseAnnotation()
    else:
      extractBaseAnnotation(baseSymbol.getImpl)

  for pImpl in baseAnnotation.implements:
    baseMethods.methodsImplemented.incl(pImpl)
    baseMethods.methodsAbstract.excl(pImpl)
  for pAbstract in baseAnnotation.abstracts:
    baseMethods.methodsAbstract.incl(pAbstract)


# -----------------------------------------------------------------------------
# Class name parsing (mainly needed to split generic params)
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
# Constructor parsing
# -----------------------------------------------------------------------------

type
  Constructor = ref object
    name: Option[string]
    args: seq[NimNode]

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
    origNode: NimNode

proc isBaseCall(n: NimNode): bool =
  n.kind == nnkCall and n[0].kind == nnkIdent and n[0].strVal == "base"

proc parseBaseCall(n: NimNode): ParsedBaseCall =
  var args = newSeq[NimNode]()
  for i in 1 ..< n.len:
    args.add(n[i])
  ParsedBaseCall(args: args, origNode: n)

# -----------------------------------------------------------------------------
# Post init block parsing
# -----------------------------------------------------------------------------

proc isPostInitBlock(n: NimNode): bool =
  n.kind == nnkCall and n[0].kind == nnkIdent and n[0].strVal == "postInit"

proc parsePostInitBlock(n: NimNode): NimNode =
  n[1]

# -----------------------------------------------------------------------------
# Proc parsing
# -----------------------------------------------------------------------------

type
  ParsedProc = ref object of RootObj

  ExportedProc = ref object of ParsedProc
    name: string
    procDef: NimNode
    isAbstract: bool
    isOverride: bool
    fieldDef: NimNode

  PrivateProc = ref object of ParsedProc
    name: string
    procDef: NimNode


proc convertProcDefIntoField(procdef: NimNode): NimNode =
  # We need to turn funcName into funcName* for export
  let procIdent = procdef[0]
  let field = publicIdent(procIdent.strVal)
  let fieldType = newNimNode(nnkProcTy).add(
    procdef[3], # copy formal params
    newEmptyNode(),
  )
  result = newIdentDefs(field, fieldType)


proc findPragma(n: NimNode, pragma: string): bool =
  for child in n:
    if child.kind == nnkIdent and child.strVal == pragma:
      return true
  return false


proc parseProcDef(procDef: NimNode): ParsedProc =
  expectKind procDef, nnkProcDef
  if procDef[0].kind == nnkPostfix and procDef[0][0].strVal == "*":
    let name = procDef[0][1].strVal
    let isAbstract = procDef.body.kind == nnkEmpty
    let isOverride = procDef.pragmas.findPragma("override")
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
      isOverride: isOverride,
      fieldDef: convertProcDefIntoField(transformedProcDef)
    )
  else:
    result = PrivateProc(
      name: procDef[0].strVal,
      procDef: procDef, # .copyNimTree(),
    )

# -----------------------------------------------------------------------------
# Body parsing
# -----------------------------------------------------------------------------

type
  BodyStmt = ref object of RootObj
  BodyStmtNode = ref object of BodyStmt
    node: NimNode
  BodyStmtBaseCall = ref object of BodyStmt
    baseCall: ParsedBaseCall

  ParsedBody = ref object
    hasBaseCall: bool
    ctor: Option[Constructor]
    exportedProcs: seq[ExportedProc]
    privateProcs: seq[PrivateProc]
    bodyStmts: seq[BodyStmt]
    postInitBlock: Option[NimNode]

proc parseBody(body: NimNode): ParsedBody =
  result = ParsedBody(
    hasBaseCall: false,
    ctor: none(Constructor),
  )
  for n in body:
    if n.kind == nnkProcDef:
      let parsedProc = parseProcDef(n)
      if parsedProc of ExportedProc:
        result.exportedProcs.add(parsedProc.ExportedProc)
      elif parsedProc of PrivateProc:
        result.privateProcs.add(parsedProc.PrivateProc)
    elif n.isConstructor():
      if result.ctor.isNone:
        result.ctor = some(n.parseConstructor())
      else:
        error "Class definition must have only one constructor.", n
    elif n.isBaseCall():
      if not result.hasBaseCall:
        result.hasBaseCall = true
        result.bodyStmts.add(
          BodyStmtBaseCall(baseCall: n.parseBaseCall())
        )
      else:
        error "Class definition must have only one base call.", n
    elif n.isPostInitBlock():
      if result.postInitBlock.isNone:
        result.postInitBlock = some(n.parsePostInitBlock)
      else:
        error "Class definition must have only one post init block.", n
    else:
      result.bodyStmts.add(
        BodyStmtNode(node: n.copyNimTree())
      )
      # error "Disallowed node in class definition:\n" & n.repr, n

# -----------------------------------------------------------------------------
# Overload analysis
# -----------------------------------------------------------------------------

type
  OverloadInfo = ref object
    isAbstract: bool
    isFullyOverloaded: bool
    newProcs: seq[ExportedProc]
    overloadedProcs: seq[ExportedProc]
    remainingBaseProcs: seq[string]

proc overloadAnalysis(baseProcs: BaseMethods, exportedProcs: seq[ExportedProc]): OverloadInfo =
  ## Generates the overload information
  result = OverloadInfo(isAbstract: false)

  # helper sets
  var baseProcsNameSet = initHashSet[string]()
  for p in baseProcs.methodsAbstract:
    baseProcsNameSet.incl(p)
  for p in baseProcs.methodsImplemented:
    baseProcsNameSet.incl(p)
  var overloadedProcsNameSet = initHashSet[string]()

  for exportedProc in exportedProcs:
    # Any abstract proc defined on self makes it abstract
    if exportedProc.isAbstract:
      result.isAbstract = true
    if baseProcsNameSet.contains(exportedProc.name):
      if not exportedProc.isOverride:
        error &"Method '{exportedProc.name}' needs {{.override.}} pragma.", exportedProc.procDef
      result.overloadedProcs.add(exportedProc)
      overloadedProcsNameSet.incl(exportedProc.name)
    else:
      if exportedProc.isOverride:
        error &"Method '{exportedProc.name}' has {{.override.}} pragma, but doesn't override anything.", exportedProc.procDef
      result.newProcs.add(exportedProc)

  # compute remaining base procs + check if there are abstracts in the remaining base procs
  for p in baseProcs.methodsImplemented:
    if not overloadedProcsNameSet.contains(p):
      result.remainingBaseProcs.add(p)
  for p in baseProcs.methodsAbstract:
    if not overloadedProcsNameSet.contains(p):
      result.remainingBaseProcs.add(p)
      result.isAbstract = true

  result.isFullyOverloaded = (result.remainingBaseProcs.len == 0)

  echo "Overload analysis"
  echo "  isAbstract:         ", result.isAbstract
  echo "  isFullyOverloaded:  ", result.isFullyOverloaded
  echo "  newProcs:           ", result.newProcs.mapIt(it.name)
  echo "  overloadedProcs:    ", result.overloadedProcs.mapIt(it.name)
  echo "  remainingBaseProcs: ", result.remainingBaseProcs

# -----------------------------------------------------------------------------
# Assembly of output procs
# -----------------------------------------------------------------------------

proc assembleTypeSection(classDef: ClassDef, baseSymbol: NimNode, overloadInfo: OverloadInfo): NimNode =
  # create type fields from exported methods
  let fields =
    if overloadInfo.newProcs.len == 0:
      newEmptyNode()
    else:
      let reclist = newNimNode(nnkRecList)
      for exportedProc in overloadInfo.newProcs:
        reclist.add(exportedProc.fieldDef)
      reclist

  # helper for the pragma expression
  proc makeSeq(names: seq[string]): NimNode =
    let bracket = newNimNode(nnkBracket)
    for s in names:
      bracket.add(newStrLitNode(s))
    newNimNode(nnkPrefix).add(
      ident "@",
      bracket,
    )

  # prepare pragma expression
  var abstracts = newSeq[string]()
  var implements = newSeq[string]()
  for p in overloadInfo.newProcs:
    if p.isAbstract:
      abstracts.add(p.name)
    else:
      implements.add(p.name)
  for p in overloadInfo.overloadedProcs:
    implements.add(p.name)

  # construct pragma expression
  # Note: Strangely, when using a pragma, the object ident actually becomes
  # part of the pragma expression (i.e., the existance of the pragma moves
  # the ident down a level).
  let pragmaExpr = newNimNode(nnkPragmaExpr).add(
    publicIdent(classDef.identClass.strVal),
    newNimNode(nnkPragma).add(
      newCall(
        bindSym "implementInfo",
        makeSeq(abstracts),
        makeSeq(implements),
      )
    )
  )

  # build type section
  let typeSection = newNimNode(nnkTypeSection)
  let typeDef = newNimNode(nnkTypeDef).add(
    pragmaExpr, # <-- usually ident goes here, but we have it wrapped in the pragma
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


proc assemblePatchProc(classDef: ClassDef, ctor: Constructor, baseSymbol: NimNode, baseMethods: BaseMethods, parsedBody: ParsedBody): NimNode =

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
  let selfIdent = genSym(nskParam, "self")
  result = newProc(
    publicIdent("patch"),
    [returnType, newIdentDefs(selfIdent, classDef.rawClassDef)],
  )

  # attach generic params
  if classDef.genericParams.len > 0:
    result.genericParams = newNimNode(nnkGenericParams)
    for genericParam in classDef.genericParams:
      result.genericParams.add(genericParam)

  # build closure body
  let closure = newLambda()
  closure[3] = ctorFormalParams

  # 1. ctor body (base call + var defs + init code)
  for bodyStmt in parsedBody.bodyStmts:
    matchInstance:
      case bodyStmt:
      of BodyStmtNode:
        closure.procBody.add(bodyStmt.node)
      of BodyStmtBaseCall:
        let baseCall = bodyStmt.baseCall
        # make base call
        let patchCallBase = newCall(
          newCall(
            ident "patch",
            newCall(baseSymbol, selfIdent),
          )
        )
        patchCallBase.copyLineInfo(baseCall.origNode)
        for arg in baseCall.args:
          patchCallBase.add(arg)
        closure.procBody.add(patchCallBase)

        # inject base symbol
        closure.procBody.add(
          newVarStmt(ident "base", newNimNode(nnkObjConstr).add(baseSymbol))
        )
        # Attach methods a la: `base.method = self.method`
        # Note: We attach both implemented and abstrac methods, because even
        # though calling `base.abstractMethod` does not make sense, it is
        # better that the user calls the implementation with the exception
        # instead of making an attempt to call a nil function pointer.
        for baseMethod in baseMethods.allMethods:
          closure.procBody.add(
            newAssignment(
              newDotExpr(ident "base", ident baseMethod),
              newDotExpr(selfIdent, ident baseMethod),
            )
          )

  # 2. inject `self` symbol
  template injectSelfTemplate(selfIdent) {.dirty.} =
    template self(): untyped = selfIdent
  closure.procBody.add(
    getAst(injectSelfTemplate(selfIdent))
  )

  # 3. private procs
  for privateProc in parsedBody.privateProcs:
    closure.procBody.add(
      privateProc.procDef
    )

  # 4. exported procs
  for exportedProc in parsedBody.exportedProcs:
    closure.procBody.add(
      newAssignment(
        newDotExpr(selfIdent, ident exportedProc.name),
        convertProcDefIntoLambda(exportedProc.procDef),
      )
    )

  # 5. post init block
  for postInitBlock in parsedBody.postInitBlock:
    closure.procBody.add(newBlockStmt(postInitBlock))

  result.procBody = newStmtList()
  result.procBody.add(
    newAssignment(
      ident "result",
      closure,
    )
  )
  # echo "patchProc:\n", result.treeRepr

proc assembleNamedConstructorBody(procDef: NimNode, classDef: ClassDef, ctor: Constructor) =
  # construct self
  procDef.procBody.add(
    newVarStmt(
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

  # attach formal params
  result.formalParams.add(classDef.rawClassDef) # return type
  for arg in ctor.args:
    result.formalParams.add(arg)

  # attach generic params
  if classDef.genericParams.len > 0:
    result.genericParams = newNimNode(nnkGenericParams)
    for genericParam in classDef.genericParams:
      result.genericParams.add(genericParam)

  assembleNamedConstructorBody(result, classDef, ctor)


proc assembleGenericConstructor(classDef: ClassDef, ctor: Constructor, parsedBody: ParsedBody): NimNode =
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
  body.preprocessBody()
  let parsedBody = parseBody(body)

  # recursive extraction of all base methods
  var baseMethods = BaseMethods(
    methodsAbstract: initHashSet[string](),
    methodsImplemented: initHashSet[string](),
  )
  extractBaseMethods(baseSymbol, baseMethods)
  let overloadInfo = overloadAnalysis(baseMethods, parsedBody.exportedProcs)

  # Post parse verifications here
  for ctor in parsedBody.ctor:
    if ctor.name.isSome and overloadInfo.isAbstract:
      error &"Class '{classDef.name}' cannot have a named constructor '{ctor.name.get}' because it is abstract.", body

  if not overloadInfo.isFullyOverloaded and not parsedBody.hasBaseCall:
    error &"Class '{classDef.name}' needs to have a base(...) call because it doesn't overload all parent methods.", body
  if parsedBody.hasBaseCall and baseSymbol.strVal == "RootObj":
    error &"Class '{classDef.name}' cannot have base call, because it is a root class.", body

  let ctor = parsedBody.ctor.get(Constructor(name: none(string), args: @[]))

  let typeSection = assembleTypeSection(classDef, baseSymbol, overloadInfo)
  let patchProc = assemblePatchProc(classDef, ctor, baseSymbol, baseMethods, parsedBody)

  result.add(typeSection)
  result.add(patchProc)

  # Generate constructors if not abstract
  if not overloadInfo.isAbstract:
    let genericConstructorProc = assembleGenericConstructor(classDef, ctor, parsedBody)
    result.add(genericConstructorProc)
    for name in ctor.name:
      let namedConstructorProc = assembleNamedConstructor(name, classDef, ctor, parsedBody)
      result.add(namedConstructorProc)

  # Take a copy as a work-around for: https://github.com/nim-lang/Nim/issues/10902
  result = result.copy
  echo result.repr
  #echo result.treeRepr


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
