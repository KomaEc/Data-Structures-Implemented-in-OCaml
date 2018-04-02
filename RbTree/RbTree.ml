
(* [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
let rec from i j l =
  if i>j then l
  else from i (j-1) (j::l)

(* [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
*)
let (--) i j =
  from i j []

(* [generic_of_list] is a helper function that creates a set by
 * adding all the elements of [lst] to [empty] by using [insert]. *)
let generic_of_list lst empty insert =
  List.fold_left (fun s x -> insert x s) empty lst
module RbSet = struct

  type color = Red | Blk

  (* AF:  [Leaf] represents the empty set.  [Node (_, l, v, r)] represents
   *   the set $AF(l) \union {v} \union AF(r)$. *)
  (* RI:
   * 1. for every [Node (l, v, r)], all the values in [l] are strictly
   *    less than [v], and all the values in [r] are strictly greater
   *    than [v].
   * 2. no Red Node has a Red child.
   * 3. every path from the root to a leaf has the same number of
        Blk nodes. *)
  type 'a t = Leaf | Node of (color * 'a t * 'a * 'a t)

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (_, l, v, r) ->
      if      x < v then mem x l
      else if x > v then mem x r
      else    true

  (* [balance (col, l, v, r)] implements the four possible rotations
   * that could be necessary to balance a node and restore the
   * RI clause about Red nodes. *)
  let balance = function
    | (Blk, Node (Red, Node (Red, a, x, b), y, c), z, d)
    | (Blk, Node (Red, a, x, Node (Red, b, y, c)), z, d)
    | (Blk, a, x, Node (Red, Node (Red, b, y, c), z, d))
    | (Blk, a, x, Node (Red, b, y, Node (Red, c, z, d)))
      -> Node (Red, Node (Blk, a, x, b), y, Node (Blk, c, z, d))
    | t -> Node t

  (* [insert' x t] finds the right place to insert [x] in [t], and
   * rebalances the resulting tree *)
  let rec insert' x = function
    | Leaf -> Node(Red, Leaf, x, Leaf)  (* color new node red *)
    | Node (col, l, v, r) ->
      if      x < v then balance (col, (insert' x l), v, r            )
      else if x > v then balance (col, l,             v, (insert' x r))
      else                  Node (col, l,             x, r            )

  let insert x s =
    match insert' x s with
    | Node (_, l, v, r) -> Node(Blk, l, v, r)  (* color root black *)
    | Leaf -> failwith "impossible"

(*
(* When the node's left child is a doubly black
   Four situations described in 'Introduction to Algorithms'*)
  let rec lbal = function
    | Node (Blk, dbl, y, Node (Red, Node (Blk, c, z, d), u, Node (Blk, e, v, f))) ->
      let l, d = lbal (Node (Red, dbl, y, Node (Blk, c, z, d))) in
      Node (Blk, l, u, Node (Blk, e, v, f)), d
    | Node (col, dbl, y, Node (Blk, Node (Blk, c, z, d), u, Node (Blk, e, v, f))) ->
      Node (col, dbl, y, Node (Red, Node (Blk, c, z, d), u, Node (Blk, e, v, f))), true
    | Node (col, dbl, y, Node (Blk, Node (Red, c, z, d), u, Node (Blk, e, v, f))) ->
      lbal (Node (col, dbl, y, Node (Blk, c, z, Node (Red, d, u, Node (Blk, e, v, f)))))
    | Node (col, dbl, y, Node (Blk, Node (col', c, z, d), u, Node (Red, e, v, f))) ->
      Node (col, Node (Blk, dbl, y, Node (col', c, z, d)), u, Node (Blk, e, v, f)), false

    (* analogies for situations where the doubly black node does not have a nephew *)
    | Node (Blk, dbl, y, Node (Red, Leaf, u, Leaf)) ->
      let l, d = lbal (Node (Red, dbl, y, Leaf)) in
      Node (Blk, l, u, Leaf), d
    | Node (col, dbl, y, Node (Blk, Leaf, u, Leaf)) ->
      Node (col, dbl, y, Node (Red, Leaf, u, Leaf)), true
    | Node (col, dbl, y, Node (Blk, Node (Red, c, z, d), u, Leaf)) ->
      lbal (Node (col, dbl, y, Node (Blk, c, z, Node (Red, d, u, Leaf))))
    | Node (col, dbl, y, Node (Blk, Leaf, u, Node (Red, e, v, f))) ->
      Node (col, Node (Blk, dbl, y, Leaf), u, Node (Blk, e, v, f)), false
    | _ -> failwith "impossible"

  let rec rbal = function
    | Node (Blk, Node (Red, Node (Blk, a, x, b), y, Node (Blk, c, z, d)), u, dbr) ->
      let r, d = rbal (Node (Red, Node (Blk, c, z, d), u, dbr)) in
      Node (Blk, Node (Blk, a, x, b), y, r), d
    | Node (col, Node (Blk, Node (Blk, a, x, b), y, Node (Blk, c, z, d)), u, dbr) ->
      Node (col, Node (Red, Node (Blk, a, x, b), y, Node (Blk, c, z, d)), u, dbr), true
    | Node (col, Node (Blk, Node (Blk, a, x, b), y, Node (Red, c, z, d)), u, dbr) ->
      rbal (Node (col, Node (Blk, Node (Red, Node (Blk, a, x, b), y, c), z, d), u, dbr))
    | Node (col, Node (Blk, Node (Red, a, x, b), y, Node (col', c, z, d)), u, dbr) ->
      Node (col, Node (Blk, a, x, b), y, Node (Blk, Node (col', c, z, d), u, dbr)), false

    (* analogies for situations where the doubly black node does not have a nephew *)
    | Node (Blk, Node (Red, Leaf, y, Leaf), u, dbr) ->
      let r, d = rbal (Node (Red, Leaf, u, dbr)) in
      Node (Blk, Leaf, y, r), d
    | Node (col, Node (Blk, Leaf, y, Leaf), u, dbr) ->
      Node (col, Node (Red, Leaf, y, Leaf), u, dbr), true
    | Node (col, Node (Blk, Leaf, y, Node (Red, c, z, d)), u, dbr) ->
      rbal (Node (col, Node (Blk, Node (Red, Leaf, y, c), z, d), u, dbr))
    | Node (col, Node (Blk, Node (Red, a, x, b), y, Leaf), u, dbr) ->
      Node (col, Node (Blk, a, x, b), y, Node (Blk, Leaf, u, dbr)), false
    | _ -> failwith "impossible"
    
    *)
    
   (* When the node's left child is a doubly black
   Four situations described in 'Introduction to Algorithms'*)
   let rec lbal = function
     | Node (Blk, dbl, y, Node (Red, l', u, r')) ->
       let l, d = lbal (Node (Red, dbl, y, l')) in
       Node (Blk, l, u, r'), d
     | Node (col, dbl, y, Node (Blk, Node (Red, c, z, d), u, r')) ->
       Node (col, Node (Blk, dbl, y, c), z, Node (Blk, d, u, r')), false
     | Node (col, dbl, y, Node (Blk, l', u, Node (Red, e, v, f))) ->
       Node (col, Node (Blk, dbl, y, l'), u, Node (Blk, e, v, f)), false
     | Node (col, dbl, y, Node (Blk, l', u, r')) ->
       Node (col, dbl, y, Node (Red, l', u, r')), true
     | _ -> failwith "impossible"

  let rec rbal = function
    | Node (Blk, Node (Red, l', y, r'), u, dbr) ->
      let r, d = rbal (Node (Red, r', u, dbr)) in
      Node (Blk, l', y, r), d
    | Node (col, Node (Blk, l', y, Node (Red, c, z, d)), u, dbr) ->
      Node (col, Node (Blk, l', y, c), z, Node (Blk, d, u, dbr)), false
    | Node (col, Node (Blk, Node (Red, a, x, b), y, r'), u, dbr) ->
      Node (col, Node (Blk, a, x, b), y, Node (Blk, r', u, dbr)), false
    | Node (col, Node (Blk, l', y, r'), u, dbr) ->
      Node (col, Node (Red, l', y, r'), u, dbr), true
    | _ -> failwith "impossible"


(* The boolean value d stands for "there is a doubly black"
   Remove the minimum elements in a tree *)
  let rec remove_min = function
    | Leaf
    | Node (Blk, Leaf, _, Node (Blk, _, _, _)) ->
      assert false
    | Node (Blk, Leaf, x, Leaf) ->
      Leaf, x, true
    | Node (Blk, Leaf, x, Node (Red, l, y, r)) ->
      Node (Blk, l, y, r), x, false
    | Node (Red, Leaf, x, r) ->
      r, x, false
    | Node (col, l, x, r) ->
      let l, y, d = remove_min l in
      let s = Node (col, l, x, r) in
      if d then
        let s, d = lbal s in s, y, d
      else s, y, false

  (*  Make the color of the element darker! *)
  let blackify = function
    | Node (Red, l, x, r) -> Node (Blk, l, x, r), false
    | s -> s, true


  let remove x s =
    let rec remove_aux = function
      | Leaf ->
        Leaf, false
      | Node (col, l, y, r) ->
        if x < y then
          let l, d = remove_aux l in
          let s = Node (col, l, y, r) in
          if d then lbal s else s, false
        else if x > y then
          let r, d = remove_aux r in
          let s = Node (col, l, y, r) in
          if d then rbal s else s, false
        else
          begin match r with
            | Leaf -> (match col with
                | Blk -> blackify l
                | Red -> l, false)
            | _ ->
              let r, y, d = remove_min r in
              let s = Node (col, l, y, r) in
              if d then rbal s else s, false
          end
    in fst (remove_aux s)



  let of_list lst =
    generic_of_list lst empty insert

  let rec elements = function
    | Leaf -> []
    | Node (_, l, v, r) -> (elements l) @ [v] @ (elements r)
end
