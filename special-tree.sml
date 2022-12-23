(* Tree with only leaf that have a value *)
datatype 'a btree =
    Empty
  | Leaf of 'a
  | Node of 'a btree * 'a btree
  ;

val t = Node(
  Node(Empty, Leaf 1),
  Node(Leaf 2, Leaf 3)
);

datatype 'a option = Some of 'a | None;

fun search(tree, key) =
  case tree of
    Empty => None
  | Leaf(v) => if v = key then Some(v) else None
  | Node(l, r) => case search(l, key) of
                    Some(r) => Some(r)
                  | None =>   search(r, key);

val s1 = search(t, 2);

fun sum(tree) =
  case tree of
    Empty => 0
  | Leaf(v) => v
  | Node(l, r) => sum(l) + sum(r);

val s2 = sum(t);

fun flatten(tree) =
  case tree of
    Empty => []
  | Leaf(v) => [v]
  | Node(l, r) => flatten(l) @ flatten(r);

val s3 = flatten(t);