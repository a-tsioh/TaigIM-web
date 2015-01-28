open Pcre

let ( |> )  x f = f x 

let list_of_opt_list ol =
  let rec aux l = match l with
    | [] -> []
    | (Some x)::l' -> x::aux l
    | None::l' -> aux l'
  in
  List.rev (aux ol)

let flags = [`CASELESS; `UTF8 ]

let so_of_io = function
  | Some i -> Some (string_of_int i)
  | None -> None

let string_of_option ?(none="") = function
  | None -> none
  | Some s -> s

let unique l =
  let rec aux acc l' = match l' with
    | [] -> List.rev acc
    | x::l'' -> if (List.mem x acc) then aux acc l'' else aux (x::acc) l'' 
  in aux [] l


