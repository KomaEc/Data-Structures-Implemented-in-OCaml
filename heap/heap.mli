module type Ordered = sig 
  type t 
  val compare : t -> t -> int 
end 

exception Empty 

module Make (X : Ordered) : sig 

  type t

  val create : int -> t 

  val is_empty : t -> bool 

  val add : t -> X.t -> unit 

  val remove : t -> unit 

  val minimum : t -> X.t 

  val pop : t -> X.t

  val from_array : X.t array -> t

  val fold : ('a -> X.t -> 'a) -> 'a -> t -> 'a

  val iter : (X.t -> unit) -> t -> unit

end
