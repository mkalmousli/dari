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





macro oneOfParser(
  variantNode: untyped,
  variantKindNode: untyped,
  parsers: varargs[typed]
): untyped =
  type
    VariantInfo = ref object
      name: string
      parser: NimNode
      typ: NimNode
      kindNode: NimNode
      valueNode: NimNode

  let variants = parsers.mapIt(
    block:
      let name = it[0].strVal
      let parser = it[1]
      let typ = parser.getTypeInst()[1]

      VariantInfo(
        name: name,
        parser: parser, 
        typ: typ,
        kindNode: newIdentNode("vk" & name),
        valueNode: newIdentNode(name & "Val")
      )
  )


  # dumpAstGen:
  #   type VariantKind = enum
  #     vkOne
  #     vkTwo
  let variantKindTyp = nnkTypeDef.newTree(
    variantKindNode,
    newEmptyNode(),
    nnkEnumTy.newTree(
      @[newEmptyNode()] & variants.mapIt( it.kindNode )
    )
  )


  # dumpAstGen:
  #   type Variant = ref object
  #     case kind: VariantKind
  #     of One:
  #       One: string
  #     of Two:
  #       Two: int
  let variantType = nnkTypeDef.newTree(
    variantNode,
    newEmptyNode(),
    nnkRefTy.newTree(
      nnkObjectTy.newTree(
        newEmptyNode(),
        newEmptyNode(),
        nnkRecList.newTree(
          nnkRecCase.newTree(
            @[
              nnkIdentDefs.newTree(
                newIdentNode("kind"),
                variantKindNode,
                newEmptyNode()
              )
            ] & variants.mapIt(
              nnkOfBranch.newTree(
                it.kindNode,
                nnkRecList.newTree(
                  nnkIdentDefs.newTree(
                    it.valueNode,
                    it.typ,
                    newEmptyNode()
                  )
                )
              )
            )
          )
        )
      )
    )
  )


  let inputParam = genSym(nskParam, "input")
  let parserProc = quote do:
    proc(`inputParam`: Input): Result[`variantNode`] = 
      discard
  parserProc.body.del(0)

  for v in variants:
    let p = v.parser
    let kindNode = v.kindNode
    let valueNode = v.valueNode

    parserProc.body.add quote do:
      let r = `p`(`inputParam`)
      if r.kind == rkSuccess:
        let v = `variantNode`(
          kind: `kindNode`,
          `valueNode`: r.value
        )
        return Result[`variantNode`](
          kind: rkSuccess,
          value: v,
          rest: r.rest
        )

  parserProc.body.add quote do:
    return Result[`variantNode`](
      kind: rkError,
      error: "No matching variant found"
    )

  result = nnkStmtList.newTree(
    nnkTypeSection.newTree(
      variantKindTyp,
      variantType
    ),
    parserProc
  )


if isTest:
  test "countryParser should succeed on matching country":
    let countryParser = oneOfParser(
      Country, CountryKind,
      ("Germany", tagParser("Germany")),
      ("Italy", tagParser("Italy"))
    )
    let input = Input(text: "Germany", position: 0)
    let result = countryParser(input)
    check result.kind == rkSuccess
    check result.value.kind == vkGermany

  test "countryParser should succeed on matching Italy":
    let countryParser = oneOfParser(
      Country, CountryKind,
      ("Germany", tagParser("Germany")),
      ("Italy", tagParser("Italy"))
    )
    let input = Input(text: "Italy", position: 0)
    let result = countryParser(input)
    check result.kind == rkSuccess
    check result.value.kind == vkItaly
