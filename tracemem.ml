open Types

type trace = rule list

let encode (r : rule) : int =
    match r with
      | Var -> 0
      | Lam -> 1
      | App -> 2
      | Indir -> 3
      | LamExt -> 4
      | AppExt -> 5
      | ParamExt -> 6
      | Let -> 7
      | PatternMatch -> 8
      | Maybe -> 9
      | Tuple -> 10

let decode (i : int) : rule =
    match i with
      | 0 -> Var
      | 1 -> Lam
      | 2 -> App
      | 3 -> Indir
      | 4 -> LamExt
      | 5 -> AppExt
      | 6 -> ParamExt
      | 7 -> Let
      | 8 -> PatternMatch
      | 9 -> Maybe
      | 10 -> Tuple
      | _ -> failwith "Invalid trace encoding"

(* let read_traces : unit -> trace list = 
    let result = ref [] in
    let in_channel = open_in "traces.csv" in
    try
      while true do 
        let s = input_line in_channel in
        let encoded_trace = List.map int_of_string (String.split_on_char ',' s) in
        let trace = List.map decode encoded_trace in
        result := !result @ [trace];
      done;
      close_in in_channel; 
      fun () -> !result
    with End_of_file ->
    close_in in_channel;
    fun () -> !result

let write_traces (trs : trace list) =
    let out_channel = open_out "traces.csv" in
    for i=0 to (List.length trs)-1 do
      let encoded_trace = List.map encode (List.nth trs i) in
      for j=0 to (List.length encoded_trace)-2 do
        Printf.fprintf out_channel "%d," (List.nth encoded_trace j)
      done;
      Printf.fprintf out_channel "%d\n" (List.nth encoded_trace ((List.length encoded_trace)-1))
    done;
    close_out out_channel *)