module Chapter1
    ( exercise1
    , exercise2
    , exercise3
    , exercise4_0
    , exercise4_1
    ) where

{- Exercise 1: Absolute Value
    The following is an implementation of a function for producing the absolute
    value of a number, written with an if-then-else expression. Rewrite it with
    case expression syntax.

        absVal :: (Num a, Ord a) => a -> a
        absVal x = if (x < 0) then (negate x) else x

    The parentheses around the constraints in the type signature are necessary
    when there is more than one constraint. but the parentheses around the
    expressions in the if and then clauses are not necessary. We could have
    written this instead:

        absVal x = if x < 0 then negate x else x

    But we sometimes use them anyway. Haskell’s keyword and operator precedences
    are designed to avoid the need for parentheses as much as possible, but
    there’s no harm in adding thoughtful parentheses to make it easier for
    people to parse your code.
-}
absVal :: (Num a, Ord a) => a -> a
absVal x =
    case x < 0 of
        True -> negate x
        False -> x

exercise1 = absVal

{- Exercise 2: (Bool, Bool)
    The following validation function branches on two conditions, giving four
    possible results. Rewrite it as a single case expression over
    (null username, null password).

        validateUsernamePassword :: String -> String -> String
        validateUsernamePassword username password =
            if null username
                then (if null password
                    then "Empty username and password"
                    else "Empty username")
                else (if null password
                    then "Empty password"
                    else "Okay")


    The parentheses with the comma in the form (a, b) is the syntax for a tuple.
    null is a function from the standard library that has the type [a] -> Bool
    and returns True if a list is empty and False otherwise. For example,
    (null "", null "hunter") reduces to (True, False).
-}
validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password =
    case (null username, null password) of
        (True, True)   -> "Empty username and password"
        (False, True)  -> "Empty password"
        (True, False)  -> "Empty username"
        (False, False) -> "Okay"

exercise2 = validateUsernamePassword

{- Exercise 3: A Quetion of Types
    In the standard Prelude, the head and tail list functions – for returning
    just the first element of a list or everything but the first element of
    a list, respectively – are partial: they throw exceptions on empty lists.

        head :: [a] -> a
        head [] = error "empty list"
        head (x:xs) = x

        tail :: [a] -> [a]
        tail [] = error "empty list"
        tail (x:xs) = xs

    Here we give an implementation of the tail function that does not:

        safeTail :: [a] -> [a]
        safeTail [] = []
        safeTail (x:xs) = xs


    Note the syntax in the third line, (x:xs). This deconstructs the list into
    two pieces, the head x and the tail xs. Having deconstructed it with this
    pattern, we can return just the part that we want. Later we’ll use this
    syntax again to check each element of a list, recursively.

    Consider the head function next. In the tail function above, we were able
    to avoid throwing an exception on an empty list by returning an empty list
    instead. But the following will not compile.

        safeHead :: [a] -> a
        safeHead [] = []
        safeHead (x:xs) = x

    Why doesn't it work?
-}
exercise3 :: String
exercise3 = "cannot return an empty list when the input is empty because it\
    \doesn't satisfy the return type of safeHead"

{- Exercise 4: Maybe for safety
    Write new, safe versions of head and tail
    that return Maybe values, using Nothing for the empty list cases.

        safeTail :: [a] -> Maybe [a]
        safeHead :: [a] -> Maybe a
-}
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

exercise4_0 = safeTail
exercise4_1 = safeHead
