import macros

#[
template fieldMarker(name: untyped, x: typed, access: bool): untyped =
  #discard
  if not access:
    let name: type(x) = x
  else:
    let name {.used.} : type(x) = x
]#

macro resolveType(t: typed): untyped =
  echo "here"
  echo t.treeRepr
  echo t.getType.treeRepr
  result = t.getType

template fieldMarkerPublic(name: untyped, x: typed, T: typedesc): untyped {.dirty.} =
  let name {.used.} : T = x

template fieldMarkerPrivate(name: untyped, x: typed, T: typedesc): untyped {.dirty.} =
  let name: T = x


macro doubleBody(body1: untyped, body2: typed): untyped =
  echo "\nbody1:"
  echo body1.repr
  echo "\nbody2:"
  echo body2.repr


macro singleBody(body: untyped): untyped =
  echo body.treeRepr
  result = newCall(bindSym "doubleBody", body[0], body[1])

doubleBody() do:
  echo "test"
do:
  echo "test"


let global = 42

singleBody():
  self:
    a`*` = 1
    b`+`
    c
    #d-
    #d

  block:
    echo "test"
    let x = 1
    proc test(z: string) =
      echo "global:", global
      block self:
        fieldMarkerPublic(x, x, resolveType(x))
        fieldMarkerPrivate(y, 1, resolveType(block: echo "test"; 1))
        fieldMarkerPublic(zzz, z, resolveType(z))


