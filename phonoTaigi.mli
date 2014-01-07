type syl = {
  separateur : string option;
  initial : string option;
  mediane : string option;
  finale : string option;
  ton : string option;
}

type parsing_result = Syl of syl | Other of string


module IPA :
sig
  val ipa_of_syl : ?sep:string -> syl -> string
  val ipa_of_parsing_result : parsing_result list -> string
end

module TRS :
sig
  val decompose : string -> int array
  val string_of_array : int array -> string
  val extract_tone : string -> string option * string
  val expand : string -> string
  val trs_re_imf : Pcre.regexp
  val syllable_of_trs : string -> parsing_result list
  val parse : string -> parsing_result list
  val string_of_syl : ?sep:string -> syl -> string
  val string_of_parse :
    ?sepm:string ->
    ?sepp:string -> ?discard_non_trs:bool -> parsing_result list -> string
end

module Bopomo :
sig
  val zhuyin_re : Pcre.regexp
  val string_of_syl : ?sep:string -> syl -> string
  val parse : string -> parsing_result list
  val string_of_parse :
    ?sepm:string -> ?sepp:string -> ?discard_other:bool ->  parsing_result list -> string
end

