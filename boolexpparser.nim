# TODO: use shunting-yard instead of recursive descent

import boolalg
import strutils

type
  BExpTokenKind = enum
    BTOParen, BTCParen, BTVar, BTPlus, BTBar, BTTimes, BTEnd

  BExpToken = tuple[
    kind: BExpTokenKind,
    pos: int]

  BExpTokenizer = ref object
    text: string
    pos: int
    parenDepth: int

  BExpParser = ref object
    tz: BExpTokenizer
    outputQueue: seq[BExpToken]
    opStack: seq[BExpToken]

  BExpParseError = object of Exception

proc precedence(token: BExpToken): int =
  case token.kind:
    of BTPlus: 3
    of BTTimes: 2
    of BTBar: 1
    else: 0

proc newBExpTokenizer(text: string): BExpTokenizer =
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
      of '\'':
        result.kind = BTBar
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

proc newBExpParser(tz: BExpTokenizer): BExpParser =
  new result
  result.tz = tz
  newSeq(result.outputQueue, 0)
  newSeq(result.opStack, 0)

proc processToken(p: BExpParser, token: BExpToken): bool =
  result = true
  case token.kind:
    of BTVar:
      p.outputQueue.add(token)
    of BTPlus, BTTimes, BTBar:
      while true:
        let opslen = p.opStack.len
        if opslen == 0:
          break

        let top = p.opStack[opslen - 1]

        if precedence(top) > 0 and precedence(token) > precedence(top):
          p.outputQueue.add(p.opStack.pop())
        else:
          break

      p.opStack.add(token)
    of BTOParen:
      p.opStack.add(token)
    of BTCParen:
      while true:
        if p.opStack.len == 0:
          raise newException(BExpParseError, "Unmatched close parenthesis at pos " & $token.pos)

        let top = p.opStack.pop()
        if top.kind == BTOParen:
          break
        else:
          p.outputQueue.add(top)
    of BTEnd:
      result = false

  if token.kind in {BTVar, BTBar, BTCParen} and p.tz.peek().kind in {BTVar, BTOParen}:
    discard p.processToken( (BTTimes, token.pos) )

proc processNext(p: BExpParser): bool =
  p.processToken(p.tz.next())

proc constructResult(p: BExpParser): BExp =
  var stack: seq[BExp]
  newSeq(stack, 0)

  for token in p.outputQueue:
    case token.kind:
      of BTVar:
        stack.add(v(p.tz.text[token.pos] & ""))
      of BTPlus, BTTimes:
        if stack.len < 2:
          raise newException(BExpParseError, "Missing operand for operator at pos " & $token.pos)

        let rhs = stack.pop()
        let lhs = stack.pop()

        if token.kind == BTPlus:
          stack.add(lhs + rhs)
        else:
          stack.add(lhs * rhs)
      of BTBar:
        if stack.len < 1:
          raise newException(BExpParseError, "Missing operand for bar operator at pos " & $token.pos)

        let exp = stack.pop()
        stack.add(-exp)
      else:
        raise newException(BExpParseError, "Invalid token made its way to output stack at pos " & $token.pos)

  return stack.pop()

proc parse*(p: BExpParser): BExp =
  while p.processNext(): discard

  while p.opStack.len > 0:
    p.outputQueue.add(p.opStack.pop())

  return p.constructResult()

proc parse*(text: string): BExp =
  let tz = newBExpTokenizer(text)
  let parser = newBExpParser(tz)

  return parser.parse()

when isMainModule:
  echo parse("a'b+a'c")
