
(* Resolver_lwt_unix.system doesn't seem to really handle the wss:// scheme,
 * but using https:// successfully works with the ocaml-websockets API.
 * So we substitute out the first 3 characters "wss" for "https" *)
let wss_to_https str =
    "https" ^ (Str.string_after str 3)

(* For now, just print every incoming message *)
let handle_ws_message msg =
    Lwt_io.printf "> %s\n" msg

(* TODO: better understand lwt shenanigans (specifically, why lwt-binding is
 * needed over things like sequencing with ";" *)
let connect uri =
    let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
    let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
    let%lwt client = Conduit_lwt_unix.endp_to_client ~ctx endp in
    let%lwt conn = Websocket_lwt_unix.connect ~ctx client uri in
    let rec handle_ws_input msg_content =
        let%lwt frame = Websocket_lwt_unix.read conn in
        let open Websocket in
        match frame with
        | {Frame.opcode = Ping; _} ->
                let%lwt () = Websocket_lwt_unix.write conn
                                (Websocket.Frame.create ~opcode:Pong ()) in
                handle_ws_input msg_content
        | {Frame.opcode = Pong; _} ->
                handle_ws_input msg_content
        | {Frame.opcode = Text; Frame.final = true; content; _}
        | {Frame.opcode = Binary; Frame.final = true; content; _} ->
                let%lwt () = handle_ws_message (msg_content ^ content) in
                handle_ws_input ""
        | {Frame.opcode = Text; Frame.final = false; content; _}
        | {Frame.opcode = Binary; Frame.final = false; content; _} ->
                handle_ws_input (msg_content ^ content)
        | _ -> Websocket_lwt_unix.close_transport conn in
    handle_ws_input ""

(* TODO: Think about how to better design a bot user interface for the REST/WS
 * API... *)
let run_bot () =
    let token = input_line (open_in "discord_token") in
    let uri_json = Lwt_main.run (Discaml.Gateway.get_gateway token) in
    let uri = uri_json |> (Yojson.Safe.Util.member "url") |> Yojson.Safe.Util.to_string in
    Lwt_main.run (connect (Uri.of_string (wss_to_https uri)))

let () = run_bot ()
