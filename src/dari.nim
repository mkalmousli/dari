# This is just an example to get you started. A typical binary package
# uses this file as the main entry point of the application.
include constants
import pc

proc helloWorld*(): string =
  return "Hello, World!"

# var sdsParser = tagParser("abc")
# var aParser = notParser(sdsParser)

# let r = aParser(Input(text: "b", position: 0))
# echo repr(r)