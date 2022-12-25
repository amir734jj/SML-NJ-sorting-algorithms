datatype 'a nat =
    Zero
  | Succ of 'a nat
  ;

fun to_nat(v) = if v = 0 then Zero else Succ(to_nat(v - 1));

fun plus(n1,n2) =
  case n1 of
    Zero => n2
  | Succ(v) => Succ(plus(v, n2))
    ;

fun multiply(n1,n2) =
  case n1 of
    Zero => Zero
  | Succ(v) => plus(multiply(v, n2), n2)
    ;

val five = to_nat(5);
val six = to_nat(6);
val eleven = plus(five, six);
val thirty = multiply(five, six);