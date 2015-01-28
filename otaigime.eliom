{shared{
  open Eliom_lib
  open Eliom_content
}}

module Otaigime_app =
  Eliom_registration.App (
    struct
      let application_name = "otaigime"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let main_p =
    let open Html5.D in
    p ~a:[a_class ["plop"];to_attrib (Xml.string_attrib "contenteditable" "true")] [pcdata "ceci est un debut de texte"]

let zhuyin_service =
  Eliom_registration.String.register_service
    ~path:["lookup";"zhuyin"]
    ~get_params:(Eliom_parameter.suffix (Eliom_parameter.string "input"))
    (fun input () -> Lwt.return ((TaigIME.format_list (TaigIME.request_zhuyin input)), "text/plain"))

let zhuyin_fuzzy_service =
  let open Eliom_parameter in
  Eliom_registration.String.register_service
    ~path:["lookup";"fuzzy";"zhuyin"]
    ~get_params:(suffix (int "n" ** string "input"))
    (fun (n,input) () -> Lwt.return ((TaigIME.format_list (TaigIME.request_zhuyin ~n input)), "text/plain"))


let trs_service =
  Eliom_registration.String.register_service
    ~path:["lookup";"trs"]
    ~get_params:(Eliom_parameter.suffix (Eliom_parameter.string "input"))
    (fun input () -> Lwt.return ((TaigIME.format_list (TaigIME.request_trs input)), "text/plain"))


let () =
  Otaigime_app.register
    ~service:main_service
    (fun () () ->
    {unit{
        ignore (Client.onload %main_p %Server.bus)
    }};
      Lwt.return
        (Eliom_tools.F.html
           ~title:"otaigime"
           ~css:[["css";"otaigime.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's destillery!"];
             main_p
           ])))
