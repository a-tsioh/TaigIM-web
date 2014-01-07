
open Eliom_content
open Eliom_lib


type input_text = PCDATA of string | RETURN


let zhuyin_of_charcode chr = match chr with
  | 126 -> PCDATA "～"
  | 33 -> PCDATA "！"
  | 64 -> PCDATA "＠"
  | 35 -> PCDATA "＃"
  | 36 -> PCDATA "＄"
  | 37 -> PCDATA "％"
  | 94 -> PCDATA "︿"
  | 38 -> PCDATA "＆"
  | 42 -> PCDATA "＊"
  | 40 -> PCDATA "（"
  | 41 -> PCDATA "）"
  | 95 -> PCDATA "—"
  | 43 -> PCDATA "＋"
  | 96 -> PCDATA "⋯"
  | 49 -> PCDATA "ㄅ"
  | 50 -> PCDATA "ㄉ"
  | 51 -> PCDATA "ˇ"
  | 52 -> PCDATA "ˋ"
  | 53 -> PCDATA "ㄓ"
  | 54 -> PCDATA "ˊ"
  | 55 -> PCDATA "˙"
  | 56 -> PCDATA "ㄚ"
  | 57 -> PCDATA "ㄞ"
  | 48 -> PCDATA "ㄢ"
  | 45 -> PCDATA "ㄦ"
  | 61 -> PCDATA "＝"
  | 81 -> PCDATA "q"
  | 87 -> PCDATA "w"
  | 69 -> PCDATA "e"
  | 82 -> PCDATA "r"
  | 84 -> PCDATA "t"
  | 89 -> PCDATA "y"
  | 85 -> PCDATA "u"
  | 73 -> PCDATA "i"
  | 79 -> PCDATA "o"
  | 80 -> PCDATA "p"
  | 123 -> PCDATA "『"
  | 125 -> PCDATA "』"
  | 113 -> PCDATA "ㄆ"
  | 119 -> PCDATA "ㄊ"
  | 101 -> PCDATA "ㄍ"
  | 114 -> PCDATA "ㄐ"
  | 116 -> PCDATA "ㄔ"
  | 121 -> PCDATA "ㄗ"
  | 117 -> PCDATA "ㄧ"
  | 105 -> PCDATA "ㄛ"
  | 111 -> PCDATA "ㄟ"
  | 112 -> PCDATA "ㄣ"
  | 91 -> PCDATA "「"
  | 93 -> PCDATA "」"
  | 65 -> PCDATA "a"
  | 83 -> PCDATA "s"
  | 68 -> PCDATA "d"
  | 70 -> PCDATA "f"
  | 71 -> PCDATA "g"
  | 72 -> PCDATA "h"
  | 74 -> PCDATA "j"
  | 75 -> PCDATA "k"
  | 76 -> PCDATA "l"
  | 58 -> PCDATA "："
  | 34 -> PCDATA "；"
  | 97 -> PCDATA "ㄇ"
  | 115 -> PCDATA "ㄋ"
  | 100 -> PCDATA "ㄎ"
  | 102 -> PCDATA "ㄑ"
  | 103 -> PCDATA "ㄕ"
  | 104 -> PCDATA "ㄘ"
  | 106 -> PCDATA "ㄨ"
  | 107 -> PCDATA "ㄜ"
  | 108 -> PCDATA "ㄠ"
  | 59 -> PCDATA "ㄤ"
  | 39 -> PCDATA "、"
  | 90 -> PCDATA "z"
  | 88 -> PCDATA "x"
  | 67 -> PCDATA "c"
  | 86 -> PCDATA "v"
  | 66 -> PCDATA "b"
  | 78 -> PCDATA "n"
  | 77 -> PCDATA "m"
  | 60 -> PCDATA "，"
  | 62 -> PCDATA "。"
  | 63 -> PCDATA "？"
  | 122 -> PCDATA "ㄈ"
  | 120 -> PCDATA "ㄌ"
  | 99 -> PCDATA "ㄏ"
  | 118 -> PCDATA "ㄒ"
  | 98 -> PCDATA "ㄖ"
  | 110 -> PCDATA "ㄙ"
  | 109 -> PCDATA "ㄩ"
  | 44 -> PCDATA "ㄝ"
  | 46 -> PCDATA "ㄡ"
  | 47 -> PCDATA "ㄥ"
  | 32 -> PCDATA " "
  | 13 -> RETURN
  | _ -> RETURN

let js_of_input i = match i with
  | PCDATA x -> Js.Unsafe.inject (Js.string x)
  | RETURN -> Js.Unsafe.inject (Dom_html.createBr Dom_html.document)

let unsafe_insert cnt chr sel =
  let open Js.Unsafe in
  try 
  let offset = (coerce sel)##anchorOffset in
  let anchor = (coerce sel)##anchorNode in 
  (*let cnt = (variable "window.getSelection().getRangeAt(0).endContainer") in*)
  let range = variable "document.createRange()" in 
  ignore (meth_call anchor "insertData" [|inject offset;(js_of_input chr)|];
  meth_call range "setStart" [|inject anchor;inject (offset+1)|];
  meth_call range "setEnd" [|inject anchor;inject (offset+1)|];
  meth_call sel "removeAllRanges" [||];
  meth_call sel "addRange" [|inject range|])
  with 
    _ -> ignore (
      (coerce cnt)##innerText <- (js_of_input chr);
      let range = variable "document.createRange()" in 
      let anchor = (coerce cnt)##firstChild  in
      meth_call range "setStart" (Array.map inject [|anchor;1|]);
      meth_call range "setEnd" [|inject anchor;inject (1)|];
      meth_call sel "removeAllRanges" [||];
      meth_call sel "addRange" [|inject range|]
                               
                                
                                )
    


let cb_keypress cnt bus ev thr =
  let code = zhuyin_of_charcode (ev##keyCode) in
  let sel = Dom_html.window##getSelection () in
  Dom.preventDefault ev;
  if code = RETURN then
    Js.Unsafe.(
      let txt = (coerce cnt)##innerText in
      ignore (Eliom_bus.write bus (Shared.Req (Js.to_string txt)));
      Lwt.return ()
    )
  else (
  unsafe_insert cnt code sel;
  Lwt.return ())

let conversion = function
  | Shared.Req s -> () (* Eliom_lib.alert "req %s" s*)
  | Shared.Res s -> Eliom_lib.alert "result %s" s

let onload main_p bus =
    let target = Html5.To_dom.of_p main_p in
    (Lwt_js_events.keypresses target (cb_keypress target bus) );
    Lwt.async (fun () -> Lwt_stream.iter conversion (Eliom_bus.stream bus))


