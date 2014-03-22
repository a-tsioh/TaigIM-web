val ( |> ) : 'a -> ('a -> 'b) -> 'b     
val list_of_opt_list : 'a option list -> 'a list
val flags : [> `CASELESS | `UTF8 ] list
val so_of_io : int option -> string option
val string_of_option : ?none:string -> string option -> string
val unique : 'a list -> 'a list
