#####   2.6.2
  compatible with 2.6.1
  
  add makeRegexM, makeRegexOptM, RegexResult to catch regex construction errors

#####   2.6.1
  compatible with 2.6
  
  add Format instances: ByteString, Text   
  add Trim instance: Text   
    

#####   2.6
  compatible with 2.5   
  
  overload replace (both Ascii & Utf8): add shorter arg versions  

#####   2.5
  *API changes*

  split PCRE to Ascii and Utf8

  remove \[Comp\] opt from replace signature   

  tweak trim  

#####   2.4
  refactor Replace: remove ReplaceCase. Use Once | All hints instead  
 
#####   2.3 
  include missing TestReplaceOpen 
    
#####   2.2 
  MatchSame -> MatchHint    

#####   2.1 
  *API changes*

  moved type files to Type dir   

  ReplaceOpen accepts both MatchArray and PosLen
    
#####   2.0 
  ReplaceOpen : add Text instance
    
#####   1.9 
  add ReplaceOpen. ReplaceOpen can replace in various data types in addition to String, ByteString. 

  ReplaceOpen itself does not regex. It only processes results as MatchArray. 

  Replace calls regex, then passes results to ReplaceOpen which replaces
    
#####   1.8 
  replace, replaceGroup -> replace

#####   1.7 
  add MatchSame

#####   1.6
  rollup all match fns into one class
  add Matchf, Pad

#####   1.5
  rename Match class -> ExplicitMatch

#####   1.4
  *API changes*
  changed namespace                                         

#####   1.3
  fix bug in replace
  docs
  base version constraint
  
    
there is no 1.2 version
  
#####   1.1
  refactor Replace  
  ! buggy replace

#####   1.0
  initial version 
  ! buggy replace         