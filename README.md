# regex-do

Convenience functions to search, replace, format String | ByteString with PCRE regex

example functions:

>>> format "Polly {0} a {1}" ["got","cracker"]
"Polly got a cracker"

>>> format "овчинка {a} не {b}" [("a","выделки"),("b","стоит")]
"овчинка выделки не стоит"


>>> replace [Once,Utf8] (Needle "менее", Replacement  "более") (Haystack "менее менее")
"более менее"

use replaceGroup for dynamic replace with groups in the needle

>>> split (Needle "\n") (Haystack "a\nbc\nde")
["a", "bc", "de"]


>>> trim "    aiy  \n  pdsodfg987   "
"aiy  \n  pdsodfg987"