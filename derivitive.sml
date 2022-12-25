
datatype 'a num =
    num of int
  | var of string
  | plus of ('a num * 'a num)
  | times of ('a num * 'a num)
  | power of ('a num * int)
  ;

val e1 = times(
  times(var("x"), var("x")),
  plus(var("x"), num(3))
);

val e2 = power(var("x"), 4);

fun print(e) =
  case e of
    num(v) => Int.toString(v)
  | var(x) => x
  | plus(x,y) => print(x) ^ "+" ^ print(y)
  | times(x,y) => print(x) ^ "*" ^ print(y)
  | power(x,y) => print(x) ^ "^" ^ Int.toString(y)
  ;


print(e1);
print(e2);

fun deriv(u,var(a)) =
  case u of
    num(_) => num(0)
  | var(v) => if v = a then num(1) else num(0)
  | plus(x,y) => plus(deriv((x),var(a)), deriv(y,var(a)))
  | times(x,y) => plus(times(deriv(x,var(a)), y), times(x,deriv(y,var(a))))
  | power(x,y) => times(times(num(y),power(x,y-1)), deriv(x, var(a)))
  ;

print(deriv(e1, var("x")));
print(deriv(e2, var("x")));