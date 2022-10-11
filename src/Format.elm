module Format exposing (..)

tab = "    "

newLineTab = "\n" ++ tab

indent str =
    String.replace "\n" newLineTab (tab ++ str)

indentBody str = 
    String.replace "\n" newLineTab str