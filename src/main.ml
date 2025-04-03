let home =
  let open Tyxml.Html in
  html
    (head (title (txt "Annotator")) [
        link ~rel:[`Stylesheet] ~href:"/static/index.css" ();
      ])
    (body [
        script ~a:[a_src "/static/index.js"] (txt "")
      ])

let html_to_string html =
  Format.asprintf "%a" (Tyxml.Html.pp ()) html
    
let loader _root path _request =
  match Static.read path with
  | None -> Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

let () =
  Dream.run ~error_handler:Dream.debug_error_handler
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/"
      (fun _ -> Dream.html (html_to_string home));

    Dream.get "/static/**"
      (Dream.static ~loader "");
  ]
