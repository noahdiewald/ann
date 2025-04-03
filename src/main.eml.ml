let home =
  <html>
  <body>
  <script src="/static/index.js"></script>
  </body>
  </html>
  
let loader _root path _request =
  match Static.read path with
  | None -> Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

let () =
  Dream.run ~error_handler:Dream.debug_error_handler
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/"
      (fun _ -> Dream.html home);

    Dream.get "/static/**"
      (Dream.static ~loader "");
  ]
