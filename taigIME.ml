open Pcre
open Utils
open PhonoTaigi

type syl = PhonoTaigi.syl
type word = syl list 
type s_edit =  int * syl (*cost * result *)
type w_edit =  int * word (*cost * result *)
type row = int * string * (string list)
type edit_func = syl -> s_edit option

let iflags = Pcre.cflags Utils.flags

let string_of_word w =
  String.concat "-" 
    (List.map 
       (fun s -> 
          let s' = match s.ton with
            | None -> {s with ton=Some "_"}
            | _ -> s
          in
          PhonoTaigi.IPA.ipa_of_syl ~sep:"." s') 
       w
    )



let single_edit (edit: w_edit) (func: edit_func) : w_edit list =
  let pcost,word = edit in 
  let (_,out) = List.fold_left
    (fun (before, output) current -> 
       let output' = List.map (fun (c,l) -> (c,current::l)) output in
       let output'' = match (func current) with
         | Some (cost,syl) -> (pcost + cost,(syl::before))::output'
         | None -> output'
       in
       (current::before,output'')
    )
    ([],[])
    word
  in
  List.map (fun (c,l) -> (c, List.rev l)) out



let get_more_edits edits funcs = 
  edits @ 
  (List.map 
     (fun (ed:w_edit) ->
        List.map
           (single_edit ed) funcs)
    edits
  |> List.flatten |> List.flatten) |> Utils.unique

let change_entering_tone t syl =
  match syl.finale with 
  | Some x -> None
  | None -> Some (1,{syl with finale=t})


let vocalize syl =
  match syl.initial with
  | Some "p" -> Some (1,{syl with initial=Some "b"})
  | Some "k" -> Some (1,{syl with initial=Some "g"})
  | _ -> None

let nazalise syl =
(*  let open Pcre in
  let iflags = Pcre.cflags Utils.flags in
  let rex = regexp ~iflags "([aeiou]|ɔ)$" in
  let result = exec ~rex med in*)
  let med = string_of_option syl.mediane in
  let len = String.length med in
  let letters = Utf8.to_int_array med 0 len in
  let nletters = Array.length letters in
  try 
    let last = Utf8.from_int_array  (Array.sub letters (nletters -1 ) 1) 0 1 in
    let prefix = if nletters < 2 then "" else  Utf8.from_int_array (Array.sub letters 0 (nletters -1))  0 (nletters-1) in
    match last with 
    | "a" -> Some (1,{syl with mediane=Some (prefix^"ã")})
    | "e" -> Some (1,{syl with mediane=Some (prefix^ "ẽ")})
    | "i" -> Some (1,{syl with mediane=Some (prefix^ "ĩ")})
    | "ɔ" -> Some (1,{syl with mediane=Some (prefix^ "ɔ̃")})
    | "u" -> Some (1,{syl with mediane=Some (prefix^ "ũ")})
    | _ -> None
  with _ -> None 


   


let remove_tone syl = 
  match syl.ton with
  | Some _ -> Some (1,{syl with ton=None})
  | _ -> None


let func_list = 
    remove_tone::nazalise::vocalize::(List.map 
               (fun t -> change_entering_tone t)
               [Some "t"; Some "p" ;Some "ʔ"; Some "k"])



let graphies_from_ipa dbh ipa =
  PGSQL(dbh) "SELECT graphie FROM mots WHERE ipa LIKE $ipa"

let create_table dbh =
    PGSQL(dbh) "execute" "CREATE TABLE IF NOT EXISTS mots
                         (
                           id               serial not null primary key,
                           graphie          text not null,
                           son              text not null,
                           ipa              text not null,
                           langue           text not null,
                           UNIQUE (graphie,son,langue)
                         )"


let get_candidates dbh elist =
  List.fold_left
    (fun results (c,w) -> 
       match graphies_from_ipa dbh (string_of_word w) with
       | [] -> results
       | hits ->         
         let trs = String.concat "-" (List.map (PhonoTaigi.TRS.string_of_syl) w) in
         (c,trs,hits)::results )
    []
     elist
 
let edit_list_of_parsing_results pr = 
  let rec aux l =
    match l with
    | [] -> []
    | Syl s::l' -> s::(aux l')
    | Other s::l' -> (aux l')
  in
  (0,aux pr)

let dbh = PGOCaml.connect ()

let request parse_func n input =
  let init = edit_list_of_parsing_results (parse_func input) in
  let rec extend acc n = 
    if n < 1 then acc
    else extend (get_more_edits acc func_list) (n-1) 
  in
  get_candidates dbh (extend [init] n)

let request_trs ?(n=0) =
  request PhonoTaigi.TRS.parse n

let request_zhuyin ?(n=0) =
request PhonoTaigi.Bopomo.parse n
    

let format_table rows =
  String.concat "\n"
    (List.map
       (fun (c,w,l) -> String.concat " | " (w::l))
       rows)

let format_list rows =
  let results = List.fold_left
      (fun acc (i,str,l) ->
         ("{\"sound\":\""^str^"\",\n"^
          "\"edits\":"^(string_of_int i)^",\n"^
          "\"hanji\":[\""^(String.concat "\",\"" l)^"\"]}")::acc)


(*        List.fold_left 
          (fun acc' w -> 
             if List.mem w acc' then acc'
             else w::acc' )
          acc
          l)*)
      []
      rows
  in 
  "["^
  (String.concat ",\n" results)^
  "]"

