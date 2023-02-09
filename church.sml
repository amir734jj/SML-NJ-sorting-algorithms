fun unwrap n = n (fn x => x + 1) 0;

(* Takes successor and zero as input *)
val zero = fn s => fn z => z;
val one = fn s => fn z => s z;
val two = fn s => fn z => s (s z);
val three = fn s => fn z => s (s (s z));

val succ = fn n => fn s => fn z => s (n s z);
val plus = fn n1 => fn n2 => fn s => fn z => n1 s (n2 s z);
val multiply = fn n1 => fn n2 => fn s => fn z => n2 (n1 s) z;
val pow = fn n1 => fn n2 => fn s => fn z => (n2 n1 s) z;

val unwraped3 = unwrap three;
val unwraped4 = unwrap (succ three);
val unwraped5 = unwrap (plus two three);
val unwraped6 = unwrap (multiply two three);
val unwraped8 = unwrap (pow two three);
