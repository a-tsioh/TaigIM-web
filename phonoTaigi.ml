open Utils

let iflags = Pcre.cflags Utils.flags
type syl =
  {
    separateur : string option;
    initial : string option;
    mediane : string option;
    finale  : string option;
    ton     : string option;
  }  
type parsing_result = Syl of syl | Other of string

module IPA = struct

  let ipa_of_syl ?(sep="") syl =
    let i = string_of_option syl.initial in
    let m = string_of_option syl.mediane in
    let f = string_of_option syl.finale in
    let t = string_of_option syl.ton in
    String.concat sep [i;m;f;t]

  let ipa_of_parsing_result r =
    let rec aux l = match l with
      | [] -> []
      | Syl s::l' -> (ipa_of_syl ~sep:"." s)::(aux l')
      | Other s::l' -> s::(aux l')
    in
    String.concat "-"  (aux r)



end


module TRS = struct
  open IPA

  let list_of_opt_list l =
    List.fold_left
      (fun acc el -> match el with 
         | Some x -> x::acc
         | None -> acc
      )
      []
      l
  |> List.rev

  let decompose (s:string) : int array =
    let ia = Utf8.to_int_array s 0 (String.length s) in
    Array.fold_left (fun acc x -> match Uunf.decomp x with [||] -> Array.append acc [|x|] | y -> Array.append acc y ) [||] ia

  let string_of_array (a:int array) :string =
    Utf8.from_int_array a 0 (Array.length a)

  let extract_tone syl  =
    let a = decompose syl in
    let a' = Array.fold_left
      (fun (t,acc) c -> match c with
         | 0x300 -> (Some "3", acc)
         | 0x301 -> (Some "2", acc)
         | 0x302 -> (Some "5", acc)
         | 0x304 -> (Some "7", acc)
         | 0x30d -> (Some "8", acc)
         | 0x30c -> (Some "9", acc)
         | letter -> (t, letter::acc) )
      (Some "1",[])
      a 
    in
    (fst a', string_of_array (Array.of_list (List.rev (snd a'))))
  
  let expand trs =
    let open Pcre in
    let rex = regexp ~flags "(ts|tsh|s|j)i" in
    let trs = replace_first ~rex ~templ:"$1ii" trs in
    let rex = regexp ~flags "^o([^ -o])" in
    let trs = replace ~rex ~templ:"oo$1" trs in
    let rex = regexp ~flags "([^o])o([^ -])" in
    replace ~rex ~templ:"$1oo$2" trs


  let trs_re_imf = Pcre.regexp ~flags "(p|b|ph|m|t|th|n|l|k|g|kh|ng|h|tsi|tshi|si|ts|ji|j|tsh|s)?([aeiou+]+|ng|m)(ng|nn|N|m|n|r|p|t|h|k)?"



  let convert s =
    let convert_voyels med =
      let rules = [
        ("ŋ", Pcre.regexp ~iflags "ng");
        ("ũ", Pcre.regexp ~iflags "unn");
        ("ã", Pcre.regexp ~iflags "ann");
        ("m", Pcre.regexp ~iflags "m");
        ("ĩ", Pcre.regexp ~iflags "inn");
        ("ẽ", Pcre.regexp ~iflags "enn");
        ("ɔ̃", Pcre.regexp ~iflags "oonn");
        ("ɔ", Pcre.regexp ~iflags "oo");
      ] in
      List.fold_left
        (fun s (ipa,trs) -> Pcre.replace  ~rex:trs ~templ:ipa s)
        med
        rules
    in
    let i = match s.initial with
      | Some "tsi" -> Some "tɕ"
      | Some "ji" -> Some "ʑ"
      | Some "ts" -> Some "ts"
      | Some "kh" -> Some "kʰ"
      | Some "ng" -> Some "ŋ"
      | Some "j" -> Some "dz"
      | Some "g" -> Some "g"
      | Some "ph" -> Some "pʰ"
      | Some "b" -> Some "b"
      | Some "tshi" -> Some "tɕʰ"
      | Some "h" -> Some "h"
      | Some "k" -> Some "k"
      | Some "m" -> Some "m"
      | Some "l" -> Some "l"
      | Some "th" -> Some "tʰ"
      | Some "n" -> Some "n"
      | Some "p" -> Some "p"
      | Some "s" -> Some "s"
      | Some "t" -> Some "t"
      | Some "si" -> Some "ɕ"
      | Some "tsh" -> Some "tsʰ"
      | _ -> None
    in
    let m = match s.mediane with
      |None -> None
      |Some x -> Some (convert_voyels x) 
    in
    let f = match s.finale  with
      | Some "t" -> Some "t"
      | Some "h" -> Some "ʔ"
      | Some "k" -> Some "k"
      | Some "p" -> Some "p"
      | Some "ng" -> Some "ŋ"
      | Some "m" -> Some "m"
      | Some "n" -> Some "n"
      | _ -> None
    in
    {initial=i;mediane=m;finale=f;ton=s.ton;separateur=s.separateur}



  let syllable_of_trs s =
    let open Pcre in
    let aux s =
      let i = s.(1) in
      let (m,f) = match s.(3) with
        | Some "nn" -> (Some ((string_of_option s.(2)) ^ "nn"),None)
        | _ -> (s.(2),s.(3))
      in 
      convert {separateur=None; initial=i; mediane=m; finale=f; ton=None}
    in
    let ton,syl = extract_tone s in 
    let syl = expand syl in 
    try 
      let m = (Pcre.exec ~rex:trs_re_imf syl) in
      let subs = get_opt_substrings m in
      let (debut,fin) = get_substring_ofs m 0 in
      let len = String.length syl in
      let prefix = if debut <> 0 
        then Some (Other (String.sub syl 0 debut))
        else None
      in
      let suffix = if fin <> len
        then Some (Other (String.sub syl fin (len-fin)))
        else None
      in
      list_of_opt_list 
        [prefix;  
         Some (Syl ({(aux subs) with ton=ton}));
         suffix]
    with
      Not_found -> [Other s] 

  let so_of_io = function
    |Some i -> Some (string_of_int i)
    |None -> None

  let string_of_option = function
    | Some s -> s
    | None -> ""


  let parse s =
    let open Pcre in
    let re = regexp ~flags " |--|-" in
    List.fold_left 
      (fun (delim,syls) m -> match m with
         | Text t -> (
             let parsed = syllable_of_trs t in
             List.fold_left 
               (fun (d,s) syl -> match syl with 
                  | Syl syl -> (None, (Syl {syl with separateur=d})::s)
                  | Other x -> (None, (Other ((string_of_option d)^x))::s)
               )
               (delim,syls)
               parsed
           )             
         | Delim d -> ((Some d),syls)
         | NoGroup -> raise (Invalid_argument "pb de regex, should not happen")
         | Group  _ -> raise (Invalid_argument "pb de regex, should not happen")
      )
      (None,[])
      (full_split ~rex:re  s)
  |> snd |> List.rev 


  let string_of_syl ?(sep="") syl =
    let i = match syl.initial with
      | None -> ""
      | Some s -> let len = String.length s in
        if String.get s (len-1) = 'i' then String.sub s 0 (len-1) else s
    in
    let m = string_of_option syl.mediane in
    let f = string_of_option syl.finale in
    let t = string_of_option syl.ton in
    let m' = if f <> "" then Pcre.replace ~pat:"oo" ~templ:"o" m else m in
    String.concat sep [i;m';f;t]


  let string_of_parse ?sepm:(sm="") ?sepp:(sp="") ?(discard_non_trs=false) (l:parsing_result list) : string =
    let s_of_parse_result = function
      | Other s -> if discard_non_trs then "" else s
      | Syl syl -> string_of_syl ~sep:sp syl
    in
    String.concat sm 
      (List.map s_of_parse_result l)



end

module Bopomo = struct
  let zhuyin_re = Pcre.regexp ~iflags "(--?)?(ㄅ|ㄆ|ㄇ|ㄉ|ㄊ|ㄋ|ㄌ|ㄍ|ㄎ|ㄏ|ㄐ|ㄑ|ㄒ|ㄓ|ㄔ|ㄕ|ㄖ|ㄗ|ㄘ|ㄙ)?([ㄚㄛㄜㄩㄨㄝㄧㄟ]+|ㄥ|ㄇ)([ㄇㄣㄥㄅㄉㄍㄏ])?"


let opt_apply f = function
  | None -> None
  | Some x-> Some (f x)

let normalise_zhuyin input = 
    let open Pcre in
    let rules = [
      (regexp ~iflags "ㄞ","ㄚㄧ");
      (regexp ~iflags "ㄠ","ㄚㄨ");
      (regexp ~iflags "ㄢ","ㄚㄣ");
      (regexp ~iflags "ㄤ","ㄚㄥ");
    ] in
    List.fold_left 
      (fun s (rex,templ) -> Pcre.replace ~rex ~templ s)
      input
      rules


let convert_syl s = 
  let convert_voyels med =
    let rules = [
      ("ŋ", Pcre.regexp ~iflags "ㆭ");
      ("ɔ", Pcre.regexp ~iflags "ㄛ");
      ("ũ", Pcre.regexp ~iflags "ㆫ");
      ("a", Pcre.regexp ~iflags "ㄚ");
      ("ã", Pcre.regexp ~iflags "ㆩ");
      ("e", Pcre.regexp ~iflags "ㆤ");
      ("i", Pcre.regexp ~iflags "ㄧ");
      ("m", Pcre.regexp ~iflags "ㄇ");
      ("o", Pcre.regexp ~iflags "ㄛ");
      ("ĩ", Pcre.regexp ~iflags "ㆪ");
      ("u", Pcre.regexp ~iflags "ㄨ");
      ("õ", Pcre.regexp ~iflags "ㆧ");
      ("ẽ", Pcre.regexp ~iflags "ㆥ");
      ("ɔ̃", Pcre.regexp ~iflags "ㆧ");
    ] in
    List.fold_left
      (fun s (ipa,zhuyin) -> Pcre.replace  ~rex:zhuyin ~templ:ipa s)
      med
      rules
  in
  let i = match s.initial with
    | Some "ㄐ" -> Some "tɕ"
    | Some "ㆢ" -> Some "ʑ"
    | Some "ㄗ" -> Some "ts"
    | Some "ㄎ" -> Some "kʰ"
    | Some "ㄫ" -> Some "ŋ"
    | Some "ㆡ" -> Some "dz"
    | Some "ㆣ" -> Some "g"
    | Some "ㄆ" -> Some "pʰ"
    | Some "ㆠ" -> Some "b"
    | Some "ㄑ" -> Some "tɕʰ"
    | Some "ㄏ" -> Some "h"
    | Some "ㄍ" -> Some "k"
    | Some "ㄇ" -> Some "m"
    | Some "ㄌ" -> Some "l"
    | Some "ㄊ" -> Some "tʰ"
    | Some "ㄋ" -> Some "n"
    | Some "ㄅ" -> Some "p"
    | Some "ㄙ" -> Some "s"
    | Some "ㄉ" -> Some "t"
    | Some "ㄒ" -> Some "ɕ"
    | Some "ㄘ" -> Some "tsʰ"
    | _ -> None
  in
  let m = match s.mediane with
    |None -> None
    |Some x -> Some (convert_voyels x) 
  in
  let f = match s.finale  with
    | Some "ㆵ" -> Some "t"
    | Some "ㆷ" -> Some "ʔ"
    | Some "ㆶ" -> Some "k"
    | Some "ㆴ" -> Some "p"
    | Some "ㄥ" -> Some "ŋ"
    | Some "ㄇ" -> Some "m"
    | Some "ㄣ" -> Some "n"
    | Some "ㄅ" -> Some "p"
    | Some "ㄉ" -> Some "t"
    | Some "ㄍ" -> Some "k"
    | Some "ㄏ" -> Some "ʔ"
    | _ -> None
  in
  {initial=i;mediane=m;finale=f;ton=s.ton;separateur=s.separateur}

let convert w =
  List.map
    convert_syl
    w


let filter_option l =
  let rec aux l' = match l' with
    | [] -> []
    | (Some x)::l'' -> x::(aux l'')
    | None::l'' -> aux l''
  in
  List.rev (aux l)

let syllable_of_zhuyin input =
  let open IPA in 
  let open Pcre in
  let aux s =
    let sep = s.(1) in
    let i = s.(2) in
    let m = opt_apply normalise_zhuyin (s.(3)) in
    let f = s.(4) in
    convert_syl {separateur=sep; initial=i; mediane=m; finale=f; ton=None}
  in
  try 
    let m = (Pcre.exec  ~rex:zhuyin_re input) in
    let subs = get_opt_substrings m in
    let (debut,fin) = get_substring_ofs m 0 in
    let len = String.length input in
    let prefix = if debut <> 0 
      then Some (Other (String.sub input 0 debut))
      else None
    in
    let suffix = if fin <> len
      then Some (Other (String.sub input fin (len-fin)))
      else None
    in
    filter_option
      [prefix;  
       Some (Syl ({(aux subs) with ton=None}));
       suffix]
  with
    Not_found -> [Other input] 

let parse s =
  let s = normalise_zhuyin s in
  let new_syl s =
    let sep = s.(1) in
    let i = s.(2) in
    let m = s.(3) in
    let f = s.(4) in
    convert_syl {separateur=sep; initial=i; mediane=m; finale=f; ton=None}
  in
  let open Pcre in
  let rec find_next s acc =
    try
      let sub = exec ~rex:zhuyin_re s in
      let (start,finish) = Pcre.get_substring_ofs sub 0 in 
      let acc' = 
        if start > 0 then (Other (String.sub s 0 start))::acc
        else acc
      in
      let len = String.length s in
      let syl = Syl (new_syl (Pcre.get_opt_substrings sub)) in
      if len = finish 
      then List.rev (syl::acc')
      else find_next (String.sub s finish (len-finish)) (syl::acc')
    with
    | Not_found -> List.rev ((Other s)::acc)
  in
  find_next s []



let string_of_syl ?(sep="") syl = ""

let discard_non_zhuyin l =
  List.map (function (Syl s) -> Some s | _ -> None) l |> filter_option

let string_of_parse ?sepm:(sm="") ?sepp:(sp="") ?(discard_other=false) (l:parsing_result list) : string =
  let s_of_parse_result = function
    | Other s -> if discard_other then "" else s
    | Syl syl -> IPA.ipa_of_syl ~sep:sp syl
  in
  String.concat sm 
    (List.map s_of_parse_result l)



end


