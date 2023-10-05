type 'a t = ()

let empty = ()
let add set elem = ()
let remove set elem = ()
let contains set elem = false

module type Comparable = sig
  type t
  type result = Less | Greater | Equal

  val compare : t -> t -> result
end

module AVL (T : Comparable) = struct
  type t = Leaf | Node of T.t * T.t
end
