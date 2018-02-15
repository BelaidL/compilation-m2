def init_array(n) = new[n]

val res =
  let a = init_array(10) in
  a[0] := 12;
  a[0]
