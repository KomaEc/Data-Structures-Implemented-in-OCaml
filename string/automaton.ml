

let r = 256

let char_at s i = String.get s i |> int_of_char

module type S = 
sig
  val pattern : string
end

module Make (X : S) = 
struct

  let length = String.length X.pattern

  let dfa = Array.make_matrix r length 0

  (* initialization *)
  let () = 
  let x = ref 0 in (* place that match ... i-1 *)
    begin
      dfa.(char_at X.pattern 0).(0) <- 1;
      for i = 1 to length - 1 do
        for r' = 0 to r - 1 do
          dfa.(r').(i) <- dfa.(r').(!x);
        done;
        dfa.(char_at X.pattern i).(i) <- i + 1;
        x := dfa.(char_at X.pattern i).(!x)
      done
    end

  let find (target : string) : int = 
    let length' = String.length target
    and i = ref 0 
    and j = ref 0 in
    begin
      while !i < length' && !j < length do 
        j := dfa.(char_at target !i).(!j);
        incr i
      done
    end;
    if !j == length then !i - length
    else length'

end

module Test() = struct 

  let test1 () = 
    let module K = Make(struct let pattern = "AAAAA" end) in 
    let open K in 
    assert (find "AAAABAAAABAAAABAAAAA" == 15)
    
end