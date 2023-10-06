type 'a t = unit

open Base

let empty = ()
let add set elem = ()
let remove set elem = ()
let contains set elem = false

type result = Less | Greater | Equal

module type Comparable = sig
  type t

  val compare : t -> t -> result
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

module AVL (T : Comparable) = struct
  type t = int * t' [@@deriving sexp]
  and t' = Leaf | Node of t * T.t * t [@@deriving sexp]

  let empty : t = (0, Leaf)

  exception BugRotate of t [@@deriving sexp]

  let rec maxdepth t =
    match snd t with
    | Leaf -> 0
    | Node (l, _, r) -> 1 + max (maxdepth l) (maxdepth r)

  let rec exists t v =
    match t with
    | _, Leaf -> false
    | _, Node (l, v', r) -> (
        match T.compare v v' with
        | Less -> exists l v
        | Greater -> exists r v
        | Equal -> true)

  let rec iter t ~f =
    match snd t with
    | Leaf -> ()
    | Node (l, v, r) ->
        iter l ~f;
        f v;
        iter r ~f

  let insert t v =
    let rot_right t =
      match t with
      | _, Node ((bl, Node (ll, vl, lr)), v, r) ->
          (1 - bl, Node (ll, vl, (-1 - bl, Node (lr, v, r))))
      | _ -> raise (BugRotate t)
    in
    let rot_left t =
      match t with
      | _, Node (l, v, (br, Node (rl, vr, rr))) ->
          (-1 - br, Node ((1 - br, Node (l, v, rl)), vr, rr))
      | _ -> raise (BugRotate t)
    in
    let rot_right_left t =
      match t with
      | b, Node (l, v, r) -> rot_left (b, Node (l, v, rot_right r))
      | _ -> raise (BugRotate t)
    in
    let rot_left_right t =
      match t with
      | b, Node (l, v, r) -> rot_right (b, Node (rot_left l, v, r))
      | _ -> raise (BugRotate t)
    in
    let rec insert_rebalance t v : t * int =
      let maybe_violated =
        (* insert *)
        match t with
        | _, Leaf -> ((0, Node (t, v, t)), 1)
        | b, Node (l, v', r) -> (
            match T.compare v v' with
            | Less ->
                let l', hdiff = insert_rebalance l v in
                ((b - hdiff, Node (l', v', r)), if b == 1 then 0 else hdiff)
            | Greater ->
                let r', hdiff = insert_rebalance r v in
                ((b + hdiff, Node (l, v', r')), if b == -1 then 0 else hdiff)
            | Equal -> (t, 0))
      in
      match fst maybe_violated with
      | -2, Node (((0 | -1), _), _, _) -> (rot_right (fst maybe_violated), 0)
      | -2, Node ((1, _), _, _) -> (rot_left_right (fst maybe_violated), 0)
      | 2, Node (_, _, ((0 | 1), _)) -> (rot_left (fst maybe_violated), 0)
      | 2, Node (_, _, (-1, _)) -> (rot_right_left (fst maybe_violated), 0)
      | b, _ when abs b >= 2 -> raise (BugRotate (fst maybe_violated))
      | _ -> maybe_violated
    in
    fst @@ insert_rebalance t v
end

(** TESTS **)

open Stdio

module IntSet = AVL (struct
  type t = int [@@deriving sexp]

  let compare t t2 =
    match Int.compare t t2 with
    | 0 -> Equal
    | i when i < 0 -> Less
    | _ -> Greater
end)

let%test "avlEmpty" =
  let avl = List.range ~stride:(-1) 10000 0 in
  let avl =
    List.fold avl ~init:IntSet.empty ~f:(fun set i -> IntSet.insert set i)
  in
  IntSet.iter avl ~f:(fun i -> printf "%d, " i);
  printf "\ndepth: %d\n" (IntSet.maxdepth avl);
  true
