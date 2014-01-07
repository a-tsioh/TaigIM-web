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

let imageservice =
  Eliom_registration.String.register_service
    ~path:["lookup"]
    ~get_params:(Eliom_parameter.suffix (Eliom_parameter.string "input"))
    (fun input () -> Lwt.return ((TaigIME.format_table (TaigIME.request input)), "text/plain"))

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
