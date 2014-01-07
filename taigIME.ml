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
       (fun s -> PhonoTaigi.IPA.ipa_of_syl ~sep:"." {s with ton=Some "_"}) 
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


let func_list = 
  vocalize::(List.map 
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
         let trs = String.concat "-" (List.map PhonoTaigi.TRS.string_of_syl w) in
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

let request input =
  let dbh = PGOCaml.connect () in
  let init = edit_list_of_parsing_results (PhonoTaigi.Bopomo.parse input) in
  let rec extend acc n = 
    if n < 0 then acc
    else extend (get_more_edits acc func_list) (n-1) 
  in
  get_candidates dbh (extend [init] 3)


let format_table rows =
  String.concat "\n"
    (List.map
       (fun (c,w,l) -> String.concat " | " (w::l))
       rows)
