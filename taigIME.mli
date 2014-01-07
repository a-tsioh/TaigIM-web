type syl = PhonoTaigi.syl
type word = syl list 
type s_edit =  int * syl (*cost * result *)
type w_edit =  int * word (*cost * result *)
type row = int * string * (string list) (* list of candidates for a given input *)

type edit_func = syl -> s_edit option
val func_list : edit_func list

val edit_list_of_parsing_results : PhonoTaigi.parsing_result list -> w_edit
val get_more_edits : w_edit list -> edit_func list -> w_edit list
val get_candidates : (string, bool) Hashtbl.t PGOCaml.t -> w_edit list -> row list
val request : string -> row list
val format_table : row list -> string
 

