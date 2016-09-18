import sequtils

type
  BVar = distinct string

  BExpKind = enum
    BEVar, BESum, BEProd, BETrue, BEFalse, BENot

  BExp* = ref object
    case kind*: BExpKind
    of BEVar:
      bvar*: BVar
    of BESum, BEProd:
      lhs*: BExp
      rhs*: BExp
    of BETrue, BEFalse: discard
    of BENot:
      exp*: BExp

let BFalse = BExp(kind: BEFalse)
let BTrue = BExp(kind: BETrue)

proc `$`*(exp: BExp): string

proc v*(s: string): BExp = BExp(kind: BEVar, bvar: BVar(s))

proc `*`*(lhs: BExp, rhs: BExp): BExp = BExp(kind: BEProd, lhs: lhs, rhs: rhs)
proc `+`*(lhs: BExp, rhs: BExp): BExp = BExp(kind: BESum, lhs: lhs, rhs: rhs)
proc `-`*(exp: BExp): BExp = BExp(kind: BENot, exp: exp)

proc containsAll[T](s1, s2: openArray[T]): bool =
  for el in s2:
    if el notin s1:
      return false

  return true

proc setEquals[T](s1, s2: openArray[T]): bool =
  s1.len == s2.len and s1.containsAll(s2)

proc setCompare[T](s1, s2: openArray[T]): tuple[common: seq[T], extra: seq[T]] =
  newSeq(result.common, 0)
  newSeq(result.extra, 0)

  for el in s1:
    if el notin s2:
      result.extra.add(el)
    else:
      result.common.add(el)

  for el in s2:
    if el notin s1:
      result.extra.add(el)

proc filterFalseTerms(terms: var seq[BExp]) =
  terms.keepItIf(it != BFalse)

proc product*(terms: seq[BExp]): BExp =
  if terms.len == 0:
    BTrue
  else:
    foldl(terms, a * b)
  
proc sum*(terms: seq[BExp]): BExp =
  if terms.len == 0:
    BFalse
  else:
    foldl(terms, a + b)
  

proc collectSumTerms(exp: BExp): seq[BExp] =
  newSeq(result, 0)
  if exp.kind == BESum:
    result.add(collectSumTerms(exp.lhs))
    result.add(collectSumTerms(exp.rhs))
  else:
    result.add(exp)

proc collectProdTerms(exp: BExp): seq[BExp] =
  newSeq(result, 0)
  if exp.kind == BEProd:
    result.add(collectProdTerms(exp.lhs))
    result.add(collectProdTerms(exp.rhs))
  else:
    result.add(exp)

proc `==`*(lhs: BExp, rhs: BExp): bool =
  if lhs.kind != rhs.kind:
    return false

  case lhs.kind:
    of BETrue, BEFalse:
      return true
    of BEVar:
      return string(lhs.bvar) == string(rhs.bvar)
    of BENot:
      return lhs.exp == rhs.exp
    of BESum, BEProd:
      # collect all terms and compare the sets
      let collectProc = if lhs.kind == BESum: collectSumTerms else: collectProdTerms
      let lhsTerms = collectProc(lhs)
      let rhsTerms = collectProc(rhs)

      if lhsTerms.len != rhsTerms.len:
        return false
      
      return lhsTerms.containsAll(rhsTerms)

proc parenthesize(exp: BExp, kinds: set[BExpKind]): string =
  if exp.kind in kinds:
    "(" & $exp & ")"
  else:
    $exp

proc `$`*(exp: BExp): string =
  case exp.kind:
    of BEVar:
      return string(exp.bvar)
    of BESum:
      return $exp.lhs & " + " & $exp.rhs
    of BEProd:
      var kinds = {BESum, BETrue, BEFalse}
      var lhsStr = parenthesize(exp.lhs, kinds)
      var rhsStr = parenthesize(exp.rhs, kinds)

      return lhsStr & rhsStr
    of BETrue:
      return "1"
    of BEFalse:
      return "0"
    of BENot:
      var str = parenthesize(exp.exp, {BESum, BEProd, BENot})
      return str & "'"

# converts POS to SOP
proc killPOS(exp: BExp): BExp =
  case exp.kind:
    of BEVar, BETrue, BEFalse:
      return exp
    of BENot:
      if exp.exp.kind == BESum:
        # Apply DeMorgan's law: (a + b)' = a'b'
        return killPOS((-killPOS(exp.exp.lhs)) * (-killPOS(exp.exp.rhs)))
      elif exp.exp.kind == BEProd:
        # Apply DeMorgan's law: (ab)' = a' + b'
        return killPOS((-killPOS(exp.exp.lhs)) + (-killPOS(exp.exp.rhs)))
      elif exp.exp.kind == BENot:
        return killPOS(exp.exp.exp);
      else:
        return -killPOS(exp.exp)
    of BESum:
      return killPOS(exp.lhs) + killPOS(exp.rhs)
    of BEProd:
      # This will distribute the product over any sums

      var lhs = killPOS(exp.lhs)
      var rhs = killPOS(exp.rhs)
      var temp: BExp

      if rhs.kind == BESum:
        discard
      elif lhs.kind == BESum:
        temp = lhs
        lhs = rhs
        rhs = temp
      else:
        # There's nothing to distribute
        return lhs * rhs

      # now rhs.kind == BESum is guaranteed
      let rrhs = rhs.rhs
      let rlhs = rhs.lhs

      return killPOS(lhs * rlhs) + killPOS(lhs * rrhs)

proc simplifyProduct(exp: BExp): BExp
proc simplifySum(exp: BExp): BExp
proc simplifySumStep(exp: BExp): tuple[s: bool, e: BExp]

proc simplify*(exp: BExp): BExp =
  let sopexp = killPOS(exp)

  case sopexp.kind:
    of BEVar, BETrue, BEFalse:
      return sopexp
    of BENot:
      return -simplify(sopexp.exp)
    of BEProd:
      return simplifyProduct(sopexp)
    of BESum:
      return simplifySum(sopexp)

proc simplifySum(exp: BExp): BExp =
  # recursively collect terms
  var terms = collectSumTerms(exp)

  # first pass: simplify, return true if a term is true, remove false terms
  for index, term in terms:
    # simplify
    terms[index] = simplify(term)
    if terms[index].kind == BETrue:
      return term

  terms = deduplicate(terms)

  # second pass: check for x + x'
  for index1, term1 in terms:
    for index2, term2 in terms:
      if index1 != index2:
        if term1.kind == BENot:
          if term2 == term1.exp:
            terms[index1] = BTrue
            terms[index2] = BTrue
        if term2.kind == BENot:
          if term1 == term2.exp:
            terms[index1] = BTrue
            terms[index2] = BTrue

  filterFalseTerms(terms)

  var success = true
  var newExp = sum(terms)

  while success:
    (success, newExp) = simplifySumStep(newExp)

  return newExp

# IMPORTANT: assumes it's not a POS, rather just product of other products
# (or just two variables)
proc simplifyProduct(exp: BExp): BExp =
  # recursively collect terms
  var terms = collectProdTerms(exp)

  # first pass: simplify, return false if a term is false, remove true terms
  for index, term in terms:
    # simplify
    terms[index] = simplify(term)
    if terms[index].kind == BEFalse:
      return term
    if terms[index].kind == BETrue:
      terms.delete(index)

  terms = deduplicate(terms)

  # second pass: check for aa'
  for index1, term1 in terms:
    for index2, term2 in terms:
      if index1 != index2:
        if term1.kind == BENot:
          if term2 == term1.exp:
            return BFalse
        if term2.kind == BENot:
          if term1 == term2.exp:
            return BFalse

  return product(terms)

proc simplifySumStep(exp: BExp): tuple[s: bool, e: BExp] =
  result.s = false
  result.e = exp

  if exp.kind != BESum:
    return

  var terms = collectSumTerms(exp).map(simplifyProduct)
  filterFalseTerms(terms)

  for index1, term1 in terms:
    for index2, term2 in terms:
      if index1 != index2:
        var terms1 = collectProdTerms(term1)
        var terms2 = collectProdTerms(term2)
        var commonTerms: seq[BExp]
        newSeq(commonTerms, 0)

        for pindex1, pterm1 in terms1:
          for pindex2, pterm2 in terms2:
            if pterm1 == pterm2:
              terms1.delete(pindex1)
              terms2.delete(pindex2)
              commonTerms.add(pterm1)

        if commonTerms.len > 0:
          let outside = product(commonTerms)
          let inside = product(terms1) + product(terms2)
          let insideSimplified = simplify(inside)

          if inside == insideSimplified:
            continue
          else:
            result.s = true
            terms[index1] = BFalse
            terms[index2] = BFalse
            terms.add(collectSumTerms(simplify(outside * insideSimplified)))
            filterFalseTerms(terms)

            result.e = sum(terms)
            return

# constraint is a list of variables (or negative variables)
# term is also that
proc checkConstraint(constraint, term: seq[BExp]): bool =
  for cterm in constraint:
    for tterm in term:
      if cterm == -tterm or -cterm == tterm:
        return false

  return true

# constraints is a list of PRODUCTS of variables (or negative variables)
# term is a list of variables (or negative variables)
proc checkImplication(constraints, term: seq[BExp]): bool =
  result = false
  for constraint in constraints:
    result = result or checkConstraint(collectProdTerms(constraint), term)

proc pruneImpliedTerms(exp: BExp): BExp =
  var terms = collectSumTerms(exp)

  var termsToDelete: seq[int]
  newSeq(termsToDelete, 0)

  for index, _ in terms:
    let deletedTerm = terms[index]
    var termscopy = terms
    termscopy.delete(index)

    let constraintExp = simplify(-sum(termscopy))
    let constraints = collectSumTerms(constraintExp)

    let deletedTermPTerms = collectProdTerms(deletedTerm)

    if not checkImplication(constraints, deletedTermPTerms):
      termsToDelete.add(index)
  
  var index = termsToDelete.len
  while index > 0:
    dec index
    terms.delete(termsToDelete[index])
  
  return sum(terms)

proc simplifyFull*(exp: BExp): BExp =
  return pruneImpliedTerms(simplify(exp))

when isMainModule:
  import boolexpparser
  import os
  echo simplifyFull(parse(paramStr(1)))


