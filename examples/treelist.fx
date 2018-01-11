
/* SOURCE OCAML:
  type 'a tree =
  | Node of 'a tree * 'a tree
  | Leaf of 'a;;

  let rec tolist t = match t with
  | Leaf v -> [v]
  | Node (a, b) -> tolist a @ tolist b;;

  let exemple = List.hd (tolist (Node (Leaf 1, Node (Leaf 2, Leaf 3))));;
*/

def nil () =
  val bk = new [1] in
  bk[0]:=0; /* tag */
  bk end

def cons (x,l) =
  val bk = new [3] in
  bk[0]:=1; /* tag */
  bk[1]:=x;
  bk[2]:=l;
  bk end

def leaf (a) =
  val bk = new [2] in
  bk[0]:=0; /* tag */
  bk[1]:=a;
  bk end

def node (g,d) =
  val bk = new [3] in
  bk[0]:=1; /* tag */
  bk[1]:=g;
  bk[2]:=d;
  bk end

def concat (l1,l2) =
  if l1[0] = 0 then l2
  else cons (l1[1], concat (l1[2],l2)) end

def tolist (t) =
  if t[0] = 0 then cons(t[1],nil())
  else concat (tolist (t[1]), tolist (t[2])) end

val ex = (tolist (node (leaf (7), node (leaf (5), leaf (9)))))[1]
/* answer: 7 */