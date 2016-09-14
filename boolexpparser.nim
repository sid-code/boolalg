# TODO: use shunting-yard instead of recursive descent

import boolalg
import strutils

type
  BExpTokenKind = enum
    BTOParen, BTCParen, BTVar, BTPlus, BTMinus, BTTimes, BTEnd

  BExpToken = tuple[
    kind: BExpTokenKind,
    pos: int]

  BExpTokenizer = ref object
    text: string
    pos: int
    parenDepth: int

  BExpParseError = object of Exception

proc newTokenizer(text: string): BExpTokenizer =
  new result
  result.text = text
  result.pos = 0
  result.parenDepth = 0

proc next(tz: BExpTokenizer, peek = false): BExpToken =
  let origPos = tz.pos
  template curch: char = tz.text[tz.pos]

  if tz.pos == tz.text.len:
    result.pos = tz.pos
    result.kind = BTEnd
  else:
    while curch in Whitespace:
      tz.pos += 1

    result.pos = tz.pos
    case curch:
      of '(':
        result.kind = BTOParen
      of ')':
        result.kind = BTCParen
      of '+':
        result.kind = BTPlus
      of '-':
        result.kind = BTMinus
      of '*':
        result.kind = BTTimes
      of 'a'..'z', 'A'..'Z':
        result.kind = BTVar
      else:
        raise newException(BExpParseError, "Invalid token '" & curch & "' at pos " & $tz.pos)

  tz.pos += 1
  if peek:
    tz.pos = origPos

proc peek(tz: BExpTokenizer): BExpToken =
  tz.next(true)

proc consume(tz: BExpTokenizer, kind: BExpTokenKind): BExpToken =
  let next = tz.next
  if next.kind != kind:
    raise newException(BExpParseError, "Expected token of kind " & $kind & ", but got " & $next.kind & " at pos " & $next.pos)
  return next

proc parseSum(tz: BExpTokenizer): BExp
proc parseProd(tz: BExpTokenizer): BExp
proc parseParenExp(tz: BExpTokenizer): BExp
proc parseSmallExp(tz: BExpTokenizer): BExp

proc parseExp(tz: BExpTokenizer): BExp =
  result = tz.parseSum()

proc parseSum(tz: BExpTokenizer): BExp =
  result = tz.parseProd()
  var tok = tz.peek()
  while tok.kind notin {BTCParen, BTEnd}:
    discard tz.consume(BTPlus)
    result = result + tz.parseProd()
    tok = tz.peek()

proc parseProd(tz: BExpTokenizer): BExp =
  template addTerm(term: BExp) =
    if isNil(result):
      result = term
    else:
      result = result * term

  var tok: BExpToken
  
  
  while true:
    tok = tz.peek()
    if tok.kind == BTMinus:
      discard tz.consume(BTMinus)
      addTerm(-tz.parseSmallExp())
    elif tok.kind == BTCParen and tz.parenDepth > 0:
      return
    elif tok.kind in {BTPlus, BTEnd}:
      return
    else:
      addTerm(tz.parseSmallExp())

proc parseParenExp(tz: BExpTokenizer): BExp =
  discard tz.consume(BTOParen)
  tz.parenDepth += 1
  result = tz.parseExp()
  discard tz.consume(BTCParen)
  tz.parenDepth -= 1

proc parseSmallExp(tz: BExpTokenizer): BExp =
  let tok = tz.peek()
  let image = tz.text[tok.pos] & "" # & "" turns it into a one character string

  if tok.kind == BTOParen:
    return tz.parseParenExp()
  elif tok.kind == BTVar:
    discard tz.consume(BTVar)
    return v(image)
  else:
    raise newException(BExpParseError, "Invalid token '" & image & "' at pos " & $tok.pos)

proc parse*(text: string): BExp =
  let tz = newTokenizer(text)
  return tz.parseExp()

when isMainModule:
  let tz = newTokenizer("(a+b)(c+d)")
  echo(tz.parseExp())
