def fact(x,r) = if x = 0 then r else fact(x-1,r*x)
val res = fact(10,1)
/* answer: 3628800 */