import macros
import strformat

proc generateUnimplementedBody*(name: string): NimNode =
  let errorMsg = &"called abstract method '{name}'"
  result = newStmtList(
    newCall(ident "doAssert", ident "false", newStrLitNode(errorMsg))
  )

