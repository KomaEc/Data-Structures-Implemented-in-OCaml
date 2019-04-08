module type Ordered = sig 
  type t 
  val compare : t -> t -> int 
end 

exception Empty 

(* minheap *)
module Make (X : Ordered) = struct 
  type t = { mutable size : int; mutable cursor : int; mutable data : X.t array }

  let create n = 
    if n <= 0 then invalid_arg "create"
    else { size = -n; cursor = 0; data = [||] }
  
  let is_empty h = h.cursor <= 0 

  let resize h = 
    let n = h.size in 
    assert (n > 0);
    let n' = 2 * n in 
    let d = h.data in 
    let d' = Array.make n' d.(0) in 
    h.size <- n';
    Array.blit d 0 d' 0 n;
    h.data <- d'

  let heapify_up h i = 
    let d = h.data in
    let x = d.(i) in
    let rec heapify_up' i = 
      let p = (i - 1) / 2 in 
      if i >= 1 && X.compare d.(p) x > 0 then 
        begin d.(i) <- d.(p);
              heapify_up' p end
      else d.(i) <- x in 
    heapify_up' i

  let heapify_down h i = 
    let d = h.data in 
    let x = d.(i) in 
    let rec heapify_down' i = 
      let j = 2 * i + 1 in 
      if j >= h.cursor then d.(i) <- x
      else let j' = j + 1 in 
           let j'' = if j' >= h.cursor || X.compare d.(j') d.(j) > 0 
                     then j else j' in 
           if X.compare x d.(j'') > 0
           then begin d.(i) <- d.(j''); heapify_down' j'' end
           else d.(i) <- x in
    heapify_down' i

  let add h elt = 
    if h.size < 0
    then begin h.size <- -h.size;
               h.data <- Array.make h.size elt end
    else if h.cursor = h.size then resize h else ();
    let n = h.cursor in
    h.data.(n) <- elt;
    h.cursor <- n + 1;
    heapify_up h n

  let minimum h = 
    if is_empty h then raise Empty
    else h.data.(0)
  
  let remove h = 
    if is_empty h then raise Empty 
    else let n = h.cursor - 1 in 
         let d = h.data in
         d.(0) <- d.(n);
         h.cursor <- n;
         heapify_down h 0
  
  let pop h = 
    let x = minimum h in remove h; x


  let from_array a = 
    let n = Array.length a in 
    let d = Array.make n a.(0) in 
    Array.blit a 0 d 0 n;
    let h = { size = n; cursor = n; data = d } in
    for i = n / 2 - 1 downto 0 do 
      heapify_down h i 
    done;
    h

  let fold f a h = 
    Array.fold_left f a h.data 

  let iter f h = 
    Array.iter f h.data
                    

end

module Test (X: sig end) = struct 
  module H = Make(struct type t = int let compare = compare end) 
  open H

  let random_array max_size = 
    let a = Array.make max_size 0 in 
    let () = Array.iteri (fun i _ -> a.(i) <- i) a in
    let rec random_permute i = 
      if i = max_size then ()
      else let delta = Random.int (max_size - i) in 
           let j = i + delta in 
           let tmp = a.(i) in 
           begin a.(i) <- a.(j);
                 a.(j) <- tmp;
                 random_permute (i + 1) end in
    random_permute 0; a

  let a = random_array 40
  let h1 = from_array a
  let rec show h = 
    if is_empty h then print_newline()
    else let m = pop h in 
         begin print_int m; print_string " ";
         show h end
  
  let () = show h1 

  let h2 = create 9

  let () = Array.iter (fun x -> add h2 x) a
  let () = show h2 

  let test1 () = 
    let a = random_array 100 in 
    let h = create 10 in 
    Array.iter (fun x -> add h x) a;
    show h

  let test2 () = 
    let a = random_array 100 in 
    let h = from_array a in 
    show h

end
