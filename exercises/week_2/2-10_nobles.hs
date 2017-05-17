-- make people noble
mknoble :: String -> String
mknoble name = "Sir " ++ name

-- make various people noble
mknoble2 :: Bool ->String -> String
mknoble2 female name = ( if female then "Dame " else "Sir ")
                          ++ name

