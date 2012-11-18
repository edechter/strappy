

draw1D = map (\i -> if i < 5 then 1 else 0) [0..10]
draw2D = map (\j -> map (\i -> if i+j == 10 then 1 else 0) [0..10]) [0..10]
--draw :: Int -> Int -> (Int -> Int -> Bool)
draw = \i -> \j -> \f -> map (\k -> map (\l -> if f k l then 1 else 0) [0..i]) [0..j]

