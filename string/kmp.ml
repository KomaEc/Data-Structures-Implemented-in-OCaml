
module Make(P : sig val pattern : string end) = 
struct 

  let m = String.length P.pattern

  let next = Array.make m (-1)

  let () = (* precomputing prefix function *)
    let open String in
    let k = ref (-1) in 
    for q = 1 to m-1 do
      while !k >= 0 && get P.pattern (!k+1) <> get P.pattern q do
        k := next.(!k)
      done;
      if get P.pattern (!k+1) = get P.pattern q then incr k;
      next.(q) <- !k
    done

  let find text = 
    let open String in
    let n = length text 
    and q = ref (-1) in
    for i = 0 to n-1 do
      while !q >= 0 && get P.pattern (!q+1) <> get text i do
        q := next.(!q)
      done;
      if get P.pattern (!q+1) = get text i then incr q;
      if !q = m-1 then 
        begin
          print_endline ("pattern found at " ^ string_of_int (i-m+1));
          assert (sub text (i-m+1) m = P.pattern);
          q := next.(!q)
        end
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