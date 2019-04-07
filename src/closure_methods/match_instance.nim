import macros
import strformat

proc makeTemplate(ident: string, typeName: string): NimNode =
  result = newNimNode(nnkTemplateDef).add(
    ident(ident),
    newEmptyNode(),
    newEmptyNode(),
    newNimNode(nnkFormalParams).add(
      ident "untyped",
    ),
    newEmptyNode(),
    newEmptyNode(),
    newStmtList(
      newCall(ident(typeName), ident(ident))
    )
  )

#macro caseInstance*(x: typed, branches: varargs[untyped]): untyped =
macro caseInstance*(x: typed, branches: untyped): untyped =
  result = newNimNode(nnkIfExpr) # newIfStmt()

  dumpTree:
    let val = if x of A:
      template x(): untyped = A(x)
    elif x of B:
      template x(): untyped = B(x)

  echo branches.treeRepr

  for branch in branches:
    echo branch.treeRepr
    if branch.kind == nnkCall:
      expectKind branch[0], nnkIdent
      expectKind branch[1], nnkStmtList
      let typeName = branch[0].strVal
      let elifBranch = newNimNode(nnkElifBranch).add(
        infix(x, "of", ident typeName),
        newStmtList(
          makeTemplate(x.strVal, typeName)
        )
      )
      for child in branch[1]:
        elifBranch[1].add(child)
      result.add(elifBranch)
    else:
      error &"Node not allowed:\n{branch.treeRepr}", branch

  echo result.repr

macro matchInstance*(body: untyped): untyped =
  echo body.treeRepr
  expectKind body[0], nnkCaseStmt
  let caseStmt = body[0]
  let caseIdent = caseStmt[0]

  result = newNimNode(nnkIfExpr) # newIfStmt()
  for i in 1 ..< caseStmt.len:
    let branch = caseStmt[i]
    if branch.kind == nnkOfBranch:
      expectKind branch[0], nnkIdent
      expectKind branch[1], nnkStmtList
      let typeName = branch[0].strVal
      let elifBranch = newNimNode(nnkElifBranch).add(
        infix(caseIdent, "of", ident typeName),
        newStmtList(
          makeTemplate(caseIdent.strVal, typeName)
        )
      )
      for child in branch[1]:
        elifBranch[1].add(child)
      result.add(elifBranch)
    elif branch.kind == nnkElse:
      result.add(branch)
    else:
      error &"Node not allowed:\n{branch.treeRepr}", branch

