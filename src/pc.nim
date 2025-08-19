import strutils
import macros
import sequtils

include constants



when isTest:
  import unittest


type
  Option*[T] = ref object
    case isSome*: bool
    of true:  
      value*: T
    else: discard

proc some*[T](value: T): Option[T] =
  result = Option[T](isSome: true, value: value)

proc none*[T](): Option[T] =
  result = Option[T](isSome: false)



type
  Input* = ref object
    text*: string
    position*: int


proc isEmpty*(input: Input): bool =
  return input.text.len == 0

proc isAtEnd*(input: Input): bool =
  return input.position >= input.text.len

proc currentChar*(input: Input): char =
  result = input.text[input.position]

proc currentString*(input: Input): string =
  if input.isAtEnd():
    return ""
  return input.text[input.position..^1]


type
  ResultKind* = enum 
    rkError
    rkSuccess

  Result*[T] = ref object
    case kind*: ResultKind
    of rkError: 
      error*: string
    of rkSuccess:
      value*: T
      rest*: Input




type
  Parser*[T] = proc(input: Input): Result[T]




proc nonEmpty*[T](parser: Parser[T]): Parser[T] =
  proc(input: Input): Result[T] =
    if input.isAtEnd():
      return Result[T](
        kind: rkError, 
        error: "Input is empty"
      )

    return parser(input)

when isTest:
  test "nonEmpty parser should return error on empty input":
    let emptyInput = Input(text: "", position: 0)
    let parser = proc(input: Input): auto =
      return Result[int](kind: rkSuccess, value: 1, rest: input)

    let result = nonEmpty(parser)(emptyInput)
    check result.kind == rkError
    check result.error == "Input is empty"





proc charParser*(c: char): Parser[char] =
  nonEmpty proc(input: Input): Result[char] =
    let currentChar = input.currentChar()
    if currentChar == c:
      return Result[char](
        kind: rkSuccess, 
        value: c, 
        rest: Input(text: input.text, position: input.position + 1)
      )

    return Result[char](
      kind: rkError, 
      error: "Expected '" & $c & "', got '" & currentChar & "'"
    )

when isTest:
  test "charParser should succeed on matching character":
    let input = Input(text: "abc", position: 0)
    let parser = charParser('a')
    let result = parser(input)
    check result.kind == rkSuccess
    check result.value == 'a'
    check result.rest.position == 1

  test "charParser should fail on non-matching character":
    let input = Input(text: "abc", position: 0)
    let parser = charParser('b')
    let result = parser(input)
    check result.kind == rkError
    check result.error == "Expected 'b', got 'a'"




proc notParser*[T](parser: Parser[T]): Parser[void] =
  proc(input: Input): Result[void] =
    let r = parser(input)
    if r.kind == rkSuccess:
      return Result[void](
        kind: rkError, 
        error: "Expected failure, got success"
      )
    return Result[void](
      kind: rkSuccess, 
      rest: input
    )

when isTest:
  test "notParser should succeed when inner parser fails":
    let input = Input(text: "abc", position: 0)
    let parser = notParser(charParser('a'))
    let result = parser(input)
    check result.kind == rkError
    check result.error == "Expected failure, got success"

  test "notParser should fail when inner parser succeeds":
    let input = Input(text: "abc", position: 0)
    let parser = notParser(charParser('b'))
    let result = parser(input)
    check result.kind == rkSuccess
    check result.rest.position == 0










proc tagParser*(tag: string): Parser[string] =
  nonEmpty proc(input: Input): Result[string] =
    let s = input.currentString()
    if s.startsWith(tag):
      return Result[string](
        kind: rkSuccess,
        value: tag,
        rest: Input(text: input.text, position: input.position + tag.len)
      )

    return Result[string](
      kind: rkError,
      error: "Expected tag '" & tag & "', got '" & input.currentString() & "'"
    )

when isTest:
  test "tagParser should succeed on matching tag":
    let input = Input(text: "abcdef", position: 0)
    let parser = tagParser("abc")
    let result: Result[system.string] = parser(input)
    check result.kind == rkSuccess
    check result.value == "abc"

  test "tagParser should fail on non-matching tag":
    let input = Input(text: "abc", position: 0)
    let parser = tagParser("def")
    let result = parser(input)
    check result.kind == rkError
    check result.error == "Expected tag 'def', got 'abc'"










macro seqParser(parsers: varargs[typed]): untyped =
  let types = parsers.mapIt(
    it.getTypeInst()[1]
  )
  
  # dumpAstGen:
  #   Result[(char, string)]
  let returnType = nnkBracketExpr.newTree(
    newIdentNode("Result"),
    nnkTupleConstr.newTree()
  )
  for t in types:
    returnType[1].add t


  var inputVar = genSym(nskParam, "input")
  result = quote do:
    proc(`inputVar`: Input): `returnType` =
      discard
  result.body.del(0)

  let resultVars = toSeq(
    0..<parsers.len()
  ).mapIt(
    genSym(nskLet, "result" & $it)
  )


  for i in 0..<parsers.len():
    let p = parsers[i]
    let resultVar = resultVars[i]

    result.body.add quote do:
      let `resultVar` = `p`(`inputVar`)
      if `resultVar`.kind == rkError:
        return `returnType`(
          kind: rkError, 
          error: `resultVar`.error
        )
    
    inputVar = quote do:
      `resultVar`.rest

  # dumpAstGen:
  #   (1, 2, 3)
  let returnValue = nnkTupleConstr.newTree()
  for resultVar in resultVars:
    returnValue.add quote do:
      `resultVar`.value


  let lastResult = resultVars[^1]
  result.body.add quote do:
    return `returnType`(
      kind: rkSuccess,
      value: `returnValue`,
      rest: `lastResult`.rest
    )

when isTest:
  test "seqParser should succeed on matching sequence":
    let p = seqParser(
      charParser('a'),
      charParser('b'),
      charParser('c')
    )

    let input = Input(text: "abc", position: 0)
    let result = p(input)
    check result.kind == rkSuccess
    check result.value == ('a', 'b', 'c')


  test "seqParser \"Hello\" + ' ' + \"World\"":
    let input = Input(text: "Hello World", position: 0)
    let p = seqParser(
      tagParser("Hello"),
      charParser(' '),
      tagParser("World")
    )
    let result = p(input)
    check result.kind == rkSuccess
    check result.value == ("Hello", ' ', "World")

