

type order = LT | EQ | GT

exception Empty

module type Ordered = sig
  type t
  val compare : t -> t -> order
end

module Int = struct
  type t = int

  let compare x y =
    if x > y then GT
    else if x < y then LT
    else EQ
end

module Make(X : Ordered) = struct
  (* AF: [size] represents the current size of the heap.
   *  [data] represents the data in the heap, using mutable
   *  to implement imperical Min-Heap. *)
  (* RI: For every index [i] of the array, [2*i], [2*i+1] are
   *  its two children (if not larger than size ). The array
   *  stores the keys, with the keys of the children are always
   *  larger than their parent, for every node.
   *)
  type t = { mutable size : int; mutable data : X.t array}


(*[resize] doubles the size of [data]*)
  let resize h =
    let n = h.size in
    assert (n > 0);
    let n' = 2 * n in
    let d = h.data in
    let d' = Array.make n' d.(0) in
    Array.blit d 0 d' 0 n ;
    h.data <- d'

  (* The usual [heapify_up] method : modify the elements at index i
 * to maintain the Heap invariant. *)
  let rec heapify_up h i =
    match i with
    | 0 -> ()
    | _ ->
      let p = i / 2 in
      let x = h.data.(p) in
      match X.compare h.data.(p) h.data.(i) with
      | GT ->
        begin
          h.data.(p) <- h.data.(i);
          heapify_up h p ;
          h.data.(i) <- x
        end
      | _ -> ()

  (*The usual [heapify_down] method : same with heapify_up*)
  let rec heapify_down h i =
    if 2 * i + 1 >= h.size then ()
    else let l = 2 * i + 1 in
      let r = 2 * i + 2 in
      if r >= h.size then
        let x = h.data.(l) in
        match X.compare h.data.(l) h.data.(i) with
        | LT ->
          begin
            h.data.(l) <- h.data.(i);
            heapify_down h l;
            h.data.(i) <- x
          end
        | _ ->
          ()
      else
        let c = (match X.compare h.data.(l) h.data.(r) with
            | LT -> l
            | _ -> r) in
        let x = h.data.(c) in
        match X.compare h.data.(c) h.data.(i) with
        | LT ->
          begin
            h.data.(c) <- h.data.(i);
            heapify_down h c;
            h.data.(i) <- x
          end
        | _ ->
          ()

  (* Method [startheap] receive an array or type X.t, and turn
   *  it into a Min-Heap *)
  let startheap a n =
    let h = {size=n; data=a} in
    for x = n / 2 downto 0 do
      heapify_down h x done;
    h

  (* Method [insert] first resize the array if needed, then do the usual
   * insert: put the new elements in the end of the heap, then heapify_up *)
  let insert h x =
    let n = h.size in
    if n = Array.length h.data then resize h;
    let d = h.data in begin
      d.(n) <- x;
      heapify_up h n;
      h.size <- n + 1
    end

(*Method [extract_min] take the minimum elements of the heap, put the last
  element up to the very begining and then heapify_down *)
  let extract_min h =
    if h.size <= 0 then raise Empty else
    let output = h.data.(0) in
    begin
      h.data.(0) <- h.data.(h.size - 1);
      h.size <- h.size - 1;
      heapify_down h 0;
      output
    end


end
