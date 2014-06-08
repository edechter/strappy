char i = case i of 
           0 -> "s"
           1 -> "b"
           2 -> "c"

name (i, j, k) = char i ++ char j ++ char k

line (i, j, k) = "let " ++ name (i, j, k) ++ " " 
                 ++ "f g a b c ="
                 ++ "(f" 
                 ++ (if i == 0 || i == 1 then " a " else "")
                 ++ (if j == 0 || j == 1 then " b " else "" )
                 ++ ( if k == 0 || k == 1 then " c " else "" )
                 ++ ") (g"
                 ++ (if i == 0 || i == 2 then " a " else "" )
                 ++ (if j == 0 || j == 2 then " b " else "" )
                 ++ (if k == 0 || k == 2 then " c " else "" )
                 ++")"

defs = map line [(i, j, k) | i<-[0..2], j<-[0..2], k<-[0..2]]

allNames = ["c" ++ f i ++ f j ++ f k | i<-[0..2], j<-[0..2], k<-[0..2]]
    where f 0 = "S"
          f 1 = "B"
          f 2 = "C"

                    