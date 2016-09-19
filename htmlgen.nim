import boolalg

when defined(js):
  import fakestreams
else:
  import streams

proc writeHTML*(s: Stream, simpl: BExpSimplifier) =
  s.writeline("<ol class='step-list'>")
  for step in simpl.history:
    if isNil(step.substep):
      s.write("<li class='step-item'>")
      s.write("<span class='step-comment'>")
      s.write(step.comment)
      s.write("</span><br>")
      s.write("<span class='step-res'>")
      s.write($step.res)
      s.write("</span>")
      s.writeline("</li>")
    else:
      s.writeline()
      s.writeHTML(step.substep)
  s.writeline("</ol>")

when isMainModule:
  let simpl = simplifyFull(v"a" * v"b" * v"c" + v"a" * -v"b" + v"a" * -v"c")
  newFileStream(stdout).writeHTML(simpl)
