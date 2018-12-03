module Utils exposing (..)

import Time exposing (Posix)


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


{-| Given a datetime, set the time to 00:00:00.000
-}
resetTime : Posix -> Posix
resetTime time =
    time
        |> Time.posixToMillis
        |> (\millis -> millis // (1000 * 60 * 60 * 24))
        |> (*) (1000 * 60 * 60 * 24)
        |> Time.millisToPosix
