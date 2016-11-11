2.3 >      include missing TestReplaceOpen 
    
2.2 >      MatchSame -> MatchHint    

2.1 >      *incompatible API changes*
            moved type files to Type dir   
            ReplaceOpen accepts both MatchArray and PosLen
    
2.0 >      ReplaceOpen : add Text instance
    
1.9 >     add ReplaceOpen. ReplaceOpen can replace in various data types in addition to String, ByteString. 
        ReplaceOpen itself does not regex. It only processes results as MatchArray. 
        Replace calls regex, then passes results to ReplaceOpen which replaces
    
1.8 >    replace, replaceGroup -> replace

1.7 >     add MatchSame
                                   