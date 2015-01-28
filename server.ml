open Eliom_lib
open Eliom_content

let bus = Eliom_bus.create Json.t<Shared.messages>

let perform_req = function
  | Shared.Req s -> ignore(Eliom_bus.write bus (Shared.Res (TaigIME.format_table (TaigIME.request_zhuyin s)))) 
  | Shared.Res _ -> ()

let _ =  Lwt_stream.iter perform_req (Eliom_bus.stream bus)


