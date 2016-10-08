# This code exists because Nim's JS backend doesn't have the `streams` library.
# This provides an extremely rudimentary implementation of StringStream that
# works well enough for this program's purposes.

type 
  StreamObj = object of RootObj

  Stream* = ref StreamObj

  StringStreamObj = object of Stream
    data*: string

  StringStream* = ref StringStreamObj

proc newStringStream*: StringStream =
  new result
  result.data = ""

method write*(s: Stream, data = "") = discard
method writeline*(s: Stream, data = "") = discard

method write*(s: StringStream, data = "") =
  s.data.add(data)

method writeline*(s: StringStream, data = "") =
  s.data.add(data)
  s.data.add("\n")

