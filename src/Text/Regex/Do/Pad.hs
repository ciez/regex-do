module Text.Regex.Do.Pad
    (pad,pad') where


{- | pad String with Char to total length of Int

    pad on left

 >>> pad '-' 5 "abc"

 "--abc"    -}

pad::Char -> Int -> String -> String
pad c0 tot0 txt0 = p1 ++ txt0
    where p1 = pad_ c0 tot0 txt0


{- | pad on right

    >>> pad' '-' 5 "abc"

     "abc--"        -}
pad'::Char -> Int -> String -> String
pad' c0 tot0 txt0 = txt0 ++ p1
    where p1 = pad_ c0 tot0 txt0


pad_::Char -> Int -> String -> String
pad_ c0 tot0 txt0 = [const c0 p1 | p1 <- [1..(tot0 - (length txt0))]]
