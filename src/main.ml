open Core
    
let home infile incontent =
  let open Tyxml.Html in
  html
    (head (title (txt "Annotator")) [
        link ~rel:[`Stylesheet] ~href:"/static/index.css" ();
      ])
    (body [
        div ~a:[a_user_data "infile" infile;
                a_user_data "incontent" incontent;
                a_id "dataelement";
               ] [];
        script ~a:[a_src "/static/index.js"] (txt "")
      ])

let html_to_string html =
  Format.asprintf "%a" (Tyxml.Html.pp ()) html
    
let loader _root path _request =
  match Static.read path with
  | None -> Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

let runserver inname incontent =
  Dream.run ~error_handler:Dream.debug_error_handler
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/"
      (fun _ -> Dream.html (html_to_string @@ home inname incontent));

    Dream.get "/static/**"
      (Dream.static ~loader "");
  ]
    
let () =
  let args = Sys.get_argv () in
  if Array.length args > 1 then
    let ann_text = In_channel.read_all args.(1) in
    runserver args.(1) ann_text;
    
  else
    runserver "" "";
