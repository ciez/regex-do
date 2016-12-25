# regex-do

Convenience functions to format, search, replace (String | ByteString) with PCRE regex

example functions:
```
"Polly {0} a {1}" < ["got","cracker"]
"Polly got a cracker"

"овчинка {a} не {b}" < [("a","выделки"),("b","стоит")]
"овчинка выделки не стоит"


replace (Once "менее") "более" "менее менее"
"более менее"


"\n" / "a\nbc\nde"
["a", "bc", "de"]


trim "    aiy  \n  pdsodfg987   "
"aiy  \n  pdsodfg987"
```