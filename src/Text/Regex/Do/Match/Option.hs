module Text.Regex.Do.Match.Option (
      Comp(..),
      Exec(..),
      comp,
      exec
   ) where

import Data.Bits
import qualified Text.Regex.PCRE.ByteString as B


-- | <http://www.pcre.org/pcre.txt pcre man pages>

data Comp = Blank       {- ^ 'B.compBlank'

                            clears default options: extended,caseSensitive,multiline regex
                        -}

            | Anchored  {- ^ 'B.compAnchored'

            the pattern is forced to be "anchored", that is, it
       is  constrained to match only at the first matching point in the string
       that is being searched (the "subject string").

       This effect can also  be achieved  by appropriate constructs in the pattern itself. -}

            | Caseless  {- ^ 'B.compCaseless'

            letters in the pattern match both upper  and  lower case  letters.

            It  is  equivalent  to  Perl's /i option, and it can be changed within a pattern by a (?i) option setting.

       In UTF-8 mode,  PCRE always  understands the concept of case for characters whose values are
       less than 128, so caseless matching is always possible. For  characters with  higher  values,  the concept of case is supported if PCRE is compiled with Unicode property support.
        If you want  to use  caseless  matching  for  characters 128 and above, you must ensure
       that PCRE is compiled with Unicode property support  as  well  as  with UTF-8 support.   -}

            | Dotall    {- ^ 'B.compDotAll'

            a dot metacharacter in the pattern matches a  character of any value,
            including one that indicates a newline.

            However, it only ever matches one character, even if newlines are  coded  as  CRLF.

       Without  this option, a dot does not match when the current position is
       at a newline.

       This option is equivalent to Perl's /s option, and it can be  changed within a pattern by a (?s) option setting.

       A negative class such as [^a] always matches newline characters, independent of the setting of this option. -}

            | Multiline {- ^ 'B.compMultiline'

            By default, for the purposes of matching "start of line"  and  "end  of
       line", PCRE treats the subject string as consisting of a single line of
       characters, even if it actually contains newlines. The "start of  line"
       metacharacter (^) matches only at the start of the string, and the "end
       of line" metacharacter ($) matches only at the end of  the  string,  or
       before  a terminating newline (except when PCRE_DOLLAR_ENDONLY is set).
       Note, however, that unless PCRE_DOTALL  is  set,  the  "any  character"
       metacharacter  (.)  does not match at a newline. This behaviour (for ^,
       $, and dot) is the same as Perl.

       When PCRE_MULTILINE it is set, the "start of line" and  "end  of  line"
       constructs  match  immediately following or immediately before internal
       newlines in the subject string, respectively, as well as  at  the  very
       start  and  end.

       This is equivalent to Perl's /m option, and it can be
       changed within a pattern by a (?m) option setting.

       If there are no newlines  in  a  subject string, or no occurrences of ^ or $ in a pattern,
       setting PCRE_MULTILINE has no effect.    -}

            | Utf8      {- ^ 'B.compUTF8'

            This option causes PCRE to regard both the pattern and the  subject  as
       strings of UTF-8 characters instead of single-byte strings. However, it
       is available only when PCRE is built to include UTF  support.  If  not,
       the  use  of  this option provokes an error. Details of how this option
       changes the behaviour of PCRE are given in the pcreunicode page.     -}

            | Ungreedy  {- ^ 'B.compUngreedy'

            This  option  inverts  the "greediness" of the quantifiers so that they
       are not greedy by default, but become greedy if followed by "?".

       It can also be set by a (?U) option setting within the pattern.  -}
   deriving (Eq,Ord,Enum)


data Exec = BlankE  {- ^ 'B.execBlank'

                    clears default options: extended,caseSensitive,multiline regex      -}
            | NotEmpty  {- ^ 'B.execNotEmpty'

            An empty string is not considered to be a valid match if this option is
       set. If there are alternatives in the pattern, they are tried.  If  all
       the  alternatives  match  the empty string, the entire match fails. For
       example, if the pattern

         a?b?

       is applied to a string not beginning with "a" or  "b",  it  matches  an
       empty  string at the start of the subject. With PCRE_NOTEMPTY set, this
       match is not valid, so PCRE searches further into the string for occurrences of "a" or "b".        -}

            | Partial   {- ^ 'B.execPartial'

            see PCREPARTIAL(3)  in <http://www.pcre.org/pcre.txt pcre man pages>    -}
   deriving (Eq,Ord,Enum)


compOpt::Comp -> B.CompOption
compOpt o = case o of
   Blank -> B.compBlank
   Anchored -> B.compAnchored
   Caseless -> B.compCaseless
   Dotall -> B.compDotAll
   Multiline -> B.compMultiline
   Utf8 -> B.compUTF8
   Ungreedy -> B.compUngreedy


comp::[Comp] -> B.CompOption
comp [] = B.compBlank
comp l = foldl (.&.) o0 l1
   where l1 = compOpt <$> l
         o0 = compOpt $ head l


execOpt::Exec -> B.ExecOption
execOpt o =   case o of
   BlankE -> B.execBlank
   NotEmpty -> B.execNotEmpty
   Partial -> B.execPartial


exec::[Exec] -> B.ExecOption
exec [] = B.execBlank
exec l = foldl (.&.) o0 l1
   where l1 = execOpt <$> l
         o0 = execOpt $ head l