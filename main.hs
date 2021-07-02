[1,2,3] ++ [4]
take 5 [10,20..]
drop 4 "Dr. Jekyll"
take 10 (repeat 'A')
let string = "abracadabra" in take (div (length string) 2) string
let string = "rectangles" in drop 4 string
take 10 (iterate (2+) 2)
take 10 (iterate (2*) 2)
take 5 (iterate (\x -> (x+3)*2) 1)