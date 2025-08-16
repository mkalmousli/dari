# This is just an example to get you started. A typical binary package
# uses this file as the main entry point of the application.

type
  Option*[T] = ref object
    case isSome: bool
    of true:  
      value: T
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



type
  ResultKind* = enum 
    rkError
    rkSuccess

  Result*[T] = ref object
    case kind: ResultKind
    of rkError: 
      error: string
    of rkSuccess:
      value: T
      rest: Input




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


proc charParser(c: char): Parser[char] =
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


let aParser = charParser('a')
let r = aParser(Input(text: "labc", position: 0))
echo repr(r)