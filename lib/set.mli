type 'a t

val empty : 'a t
val add : 'a t -> 'a -> 'a t
val remove : 'a t -> 'a -> 'a t
val contains : 'a t -> 'a -> bool
