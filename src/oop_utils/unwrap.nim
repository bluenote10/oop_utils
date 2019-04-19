import macros

import private/utils

macro unwrap*(matchExpr: typed, body: untyped): untyped =
  echo body.treeRepr
  result = newStmtList()

  var ifBranches = newSeq[tuple[cond, body: NimNode]]()
  var elseBranch = newEmptyNode()

  for n in body:
    if n.kind == nnkInfix and n[0].isIdent("@"):
      let alias = n[1] # todo assert ident
      let mtype = n[2] # todo assert ident
      #ifBranches.add(
      #  (cond: newE)
      #)
      let tmpIdent = genSym(nskLet, "tmp")
      let letSection = newLetStmt(
        tmpIdent,
        newCall("unwrapImpl", [mtype, matchExpr])
      )
      let condVal = newDotExpr(tmpIdent, ident "isSome")
      let cond = newNimNode(nnkStmtListExpr).add(letSection, condVal)
      let body = n[3]
      body.insert(0, newLetStmt(alias, newDotExpr(tmpIdent, ident "get")))
      ifBranches.add((cond: cond, body: n[3]))

    if n.kind == nnkCall and n[0].isIdent("_"):
      elseBranch = n[1]

  let ifStmt = newIfStmt(ifBranches)
  if elseBranch.kind != nnkEmpty:
    ifStmt.add(newNimNode(nnkElse).add(elseBranch))
  result.add(ifStmt)

  echo result.repr
