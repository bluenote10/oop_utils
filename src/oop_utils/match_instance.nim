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


macro matchInstance*(body: untyped): untyped =
  ## Macro the allow simple case statement based instance matching.
  expectKind body[0], nnkCaseStmt
  let caseStmt = body[0]
  let caseIdent = caseStmt[0]

  result = newNimNode(nnkIfExpr)
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

