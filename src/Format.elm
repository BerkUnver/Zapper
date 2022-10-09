module Format exposing (..)

tab = "    "

indent str =
    String.replace "\n" ("\n" ++ tab) (tab ++ str) 