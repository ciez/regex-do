# regex-do

Convenience functions to format, search, replace (String | ByteString) with PCRE regex

example functions:
```
format "Polly {0} a {1}" ["got","cracker"]
"Polly got a cracker"

format "овчинка {a} не {b}" [("a","выделки"),("b","стоит")]
"овчинка выделки не стоит"


replace [Once,Utf8] (Pattern "менее", Replacement  "более") (Body "менее менее")
"более менее"

use replaceGroup for dynamic replace with groups in the needle

split (Pattern "\n") (Body "a\nbc\nde")
["a", "bc", "de"]


trim "    aiy  \n  pdsodfg987   "
"aiy  \n  pdsodfg987"
```