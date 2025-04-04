open Core
    
let home ann_text proj_text =
  let open Tyxml.Html in
  html
    (head (title (txt "Annotator")) [
        link ~rel:[`Stylesheet] ~href:"/static/index.css" ();
      ])
    (body [
        div ~a:[a_user_data "ann_text" ann_text;
                a_user_data "proj_text" proj_text;
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

let runserver ann_text proj_text =
  Dream.run ~error_handler:Dream.debug_error_handler
  @@ Dream.logger
  @@ Dream.livereload
  @@ Dream.router [
    Dream.get "/"
      (fun _ ->
         Dream.html (html_to_string
                     @@ home ann_text proj_text));

    Dream.get "/static/**"
      (Dream.static ~loader "");

    Dream.post "/exit"
      (fun _ -> exit 0);
    
    Dream.post "/save/:ann_file"
      (fun request ->
         let ann_file = (Dream.param request "ann_file") ^ ".json" in
         let open Lwt in
         Lwt.catch
           (fun _ ->
              Dream.body request >>=
              (fun body ->
                 Lwt_io.with_file ann_file
                   ~mode:Lwt_io.Output
                   ~flags:[O_WRONLY; O_CREAT; O_NONBLOCK]
                   ~perm:0o600
                   (fun fd -> Lwt_io.write fd body)
              ) >>= (fun _ -> Dream.respond
                        ~headers:["Content-Type", "text/plain"]
                        (ann_file ^ " saved"))
           )
           (fun _ -> Dream.respond
               ~code:500
               ~headers:["Content-Type", "text/plain"]
               "File not saved.")
      )
              
  ]
    
let () =
  let args = Sys.get_argv () in
  if Array.length args > 1 then
    let ann_file = args.(1) in
    let ann_text = In_channel.read_all ann_file in
    match Sys_unix.file_exists ("ann-project.json") with
    | `Yes ->
      let proj_text = In_channel.read_all "ann-project.json" in
      runserver ann_text proj_text;
    | _ ->
      runserver ann_text ""

  else
    runserver "" "";
