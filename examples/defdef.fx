def f(x) =
  let y =
    let z =
      if x=0 then 1 else f(x-1)
    in z+1
  in y+2
eval f(3) /* resultat: 13 */