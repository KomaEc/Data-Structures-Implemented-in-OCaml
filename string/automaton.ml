

let r = 256

module type S = 
sig
  val pattern : string
end

module Make(X : S) = 
struct
  let char_at s i = String.get s i |> int_of_char
  let m = String.length X.pattern
  let dfa = Array.make_matrix m r 0
  (** if a <> pat.(q+1), then \sigma(pat_q a) = \sigma(pat_\pi[q] a) 
    * keep in mind that if \sigma(T_i) = q, then \sigma(T_i a) = \sigma(P_q a)
    * keep k = \pi[q] *)
  let () = 
    dfa.(0).(char_at X.pattern 0) <- 1;
    let k = ref 0 in
    for q = 1 to m-1 do
      (** First calculate \pi[q] according to \pi[q-1]
        * notice that \sigma(P_{\pi[q-1]} P[q]) = \pi[q] *)
      k := dfa.(!k).(char_at X.pattern q);
      for r' = 0 to r-1 do
        dfa.(q).(r') <- dfa.(!k).(r')
      done;
      dfa.(q).(char_at X.pattern q) <- q+1
    done

  let find text = 
    let q = ref 0 
    and n = String.length text in
    let i = ref 0 in
    while !i < n && !q < m do
      q := dfa.(!q).(char_at text !i);
      incr i;
    done;
    if !q = m then 
      begin
        print_endline ("pattern found at " ^ string_of_int (!i-m));
        assert (String.sub text (!i-m) m = X.pattern);
      end

  let next = Array.make m (-1)
  let () = 
    let k = ref (-1) in 
    for q = 1 to m-1 do
      while !k <> -1 && char_at X.pattern (!k+1) <> char_at X.pattern q do
        k := next.(!k)
      done;
      if char_at X.pattern (!k+1) = char_at X.pattern q then incr k;
      next.(q) <- !k
    done
  
end

module Test() = 
struct 

  open Random

  let string_of_char_list : char list -> string = fun char_list ->
    let char_array = Array.of_list char_list in 
    String.init (Array.length char_array) (fun i -> char_array.(i))

  let rec loop cnt = 
    if cnt < 0 then () else begin
    let pat_length = 2 in 
    let text_length = Random.int 1000 + 4 + pat_length in 
    let pat_list = ref [] in
    let () = for _ = 0 to pat_length-1 do pat_list := (Random.int 26)+65 :: !pat_list done in 
    let text_list = ref [] in 
    let () = for _ = 0 to text_length-1 do text_list := (Random.int 26)+65 :: !text_list done in
    let pat = List.map char_of_int !pat_list |> string_of_char_list
    and text = List.map char_of_int !text_list |> string_of_char_list in 
    let module KMP = Make(struct let pattern = pat end) in 
    print_endline ("text : " ^ text ^ " and pattern : " ^ pat);
    KMP.find text;
    loop (cnt-1)
    end

  let run () = 
    loop 100


end
