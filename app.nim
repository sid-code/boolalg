import dom

import boolalg
import boolexpparser
import fakestreams
import htmlgen

proc simplify(str: cstring): cstring {.exportc.} =
  let exp = parse($str)
  let ss = newStringStream()

  let simpl = simplifyFull(exp)
  ss.writeHTML(simpl)

  return ss.data

proc simplifyClicked(e: Event) =
  asm """
  var str = document.getElementById("expression").value;
  var result = document.getElementById("result");
  try {
    var simplified = simplify(str);
    result.innerHTML = simplified;
  } catch (e) {
    result.innerHTML = "<span style='color:red'>" + e.stack.replace(/\n/g, '<br>') + "</span>";
  }
  """

document.addEventListener("DOMContentLoaded", proc(e: Event) =
  document.getElementByID("simplify").addEventListener("click", simplifyClicked)
  document.getElementByID("expression").addEventListener("keydown", proc(e: Event) =
    if e.keyCode == 13:
      simplifyClicked(e)))
