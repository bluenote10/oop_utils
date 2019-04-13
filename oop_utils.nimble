# Package
version       = "0.1.0"
author        = "Fabian Keller"
description   = "Macro for building OOP class hierarchies based on closure methods."
license       = "MIT"
srcDir        = "src"

# Dependencies
requires "nim >= 0.19.9"

import ospaths
import oswalkdir

import algorithm
import sequtils
import sugar
import strutils
import strformat

type
  Mode = enum
    ModeC, ModeJS

  File = string

proc getFiles(baseDir: File): seq[File] =
  result = newSeq[File]()
  for file in oswalkdir.walkDirRec(baseDir):
    let (_, name, ext) = file.splitFile()
    if ext == ".nim" and name.startswith("test"):
      result.add(file)
  result.sort((a, b) => cmp(a, b))


proc deleteRunnable(file: File, mode: Mode) =
  let binFileName =
    if mode == ModeC:
      file.changeFileExt(ExeExt)
    else:
      file.changeFileExt("js")
  rmFile(binFileName)

proc relativeTo(path: File, base: File): File =
  let baseSuffixed = base & "/"
  if path.len > baseSuffixed.len:
    path[baseSuffixed.len..^1]
  else:
    path

let colRed = "\e[1;31m"
let colGreen = "\e[1;32m"
let colYellow = "\e[1;33m"
let colBlue = "\e[1;34m"
let colWhite = "\e[1;37m"
let colReset = "\e[00m"

hint("QuitCalled", false)

proc printPassed() =
  echo &"   {colGreen}[passed]{colReset}"

proc printFailed(output: string, suffix = "") =
  echo &"   {colRed}[failed] {suffix}{colReset}"
  echo &"{colRed}------------------------------------------------{colReset}"
  echo output
  echo &"{colRed}------------------------------------------------{colReset}"

task test, "Runs the test suite":
  ## Executes all tests.
  let baseDir = system.getCurrentDir() / "tests"
  var files = getFiles(baseDir)

  if files.len < 1:
    echo &"{colRed}Warning{colReset}: No tests found!"
    quit("", 1)

  let mode = ModeC
  let compiler = if mode == ModeC: "c" else: "js"

  var numPassed = 0
  var numFailed = 0

  for file in files:
    let (_, name, _) = file.splitFile()
    let expectedError = name.startswith("testerror")

    let relativePath = file.relativeTo(baseDir)
    echo &" * {colYellow}{relativePath}{colReset}"
    let ret = gorgeEx &"nim -r -d:release --colors:on {compiler} {file}"
    deleteRunnable(file, mode)

    if not expectedError:
      if ret.exitCode == 0:
        printPassed()
        numPassed += 1
      else:
        numFailed += 1
        printFailed(ret.output)
    else:
      if ret.exitCode == 0:
        printFailed(ret.output, "test expected to fail, but passed")
        numFailed += 1
      else:
        var expectedMessage: string
        var i = 0
        for line in staticRead(file).splitLines():
          if i == 1:
            expectedMessage = line.strip()
          inc i
        if ret.output.contains(expectedMessage):
          numPassed += 1
          printPassed()
        else:
          numFailed += 1
          printFailed(ret.output, &"missing error message '{expectedMessage}'")

  if numFailed == 0:
    echo &"\n{colGreen}Success{colReset}: All {numPassed} tests passed."
  else:
    echo &"\n{colRed}Failed{colReset}: {numFailed} out of {numFailed+numPassed} tests failed."
    quit("", 1)

