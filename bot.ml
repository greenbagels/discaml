open Lwt.Infix
open Websocket_lwt_unix

let section = Lwt_log.Section.make "bot"
let last_seq = ref (-1)
let recv_hb_ack = ref true

let intents = 1024 + 512 + 1

let rec send_heartbeat conn interval =
    let open Websocket in
        let%lwt () = if !recv_hb_ack = false then
                     write conn (Frame.close 1000)
                     else Lwt.return_unit;
                     Lwt_io.printf "Sleeping for %s ms...\n" (string_of_float interval);
                     Lwt_unix.sleep (interval /. 1000.) in

        let%lwt content = if !last_seq < 0 then
            Lwt.return "{\"op\": 1, \"d\": null}"
        else Lwt.return ("{\"op\": 1, \"d\": " ^ (string_of_int !last_seq) ^ "}") in

        let%lwt () = write conn (Frame.create ~content ()) in
        recv_hb_ack := false;
        send_heartbeat conn interval

let identify conn =
    let open Websocket in
        let%lwt ic = Lwt_io.open_file ~mode:Input "discord_token" in
        let%lwt token = Lwt_io.read_line ic in
        let%lwt content = (Lwt.return ("{\"op\" : 2,
                  \"d\"  : {
                      \"token\" : \"" ^ token ^ "\",
                      \"properties\" : {
                          \"$os\" : \"a\",
                          \"$browser\" : \"b\",
                          \"$device\" : \"c\"
                          },
                      \"intents\" : " ^ (string_of_int intents) ^ "
                  }
                }")) in
        Lwt_io.printf "Sending token:\n%s\n" content;
        write conn (Frame.create ~content ())

let handle_in_content conn content =
    let open Yojson.Safe in
    let%lwt json = (Lwt.return (from_string content)) in
    let%lwt opcode = (Lwt.return json) >|= Util.member "op" >|= Util.to_int in
            match opcode with
              10 -> let%lwt interval = (Lwt.return json) >|= Util.member "d" >|= Util.member "heartbeat_interval" >|= Util.to_number in
                    send_heartbeat conn interval <?> identify conn;
                    Lwt_io.printf "Found ready opcode! Starting heartbeats\n"

            | 11 -> recv_hb_ack := true;
                    Lwt_io.printf "Received heartbeat ACK!\n"

            | _  -> Lwt_io.printf "Unknown opcode obtained!\n";
                    Lwt_io.printf "%s\n" content

let client uri =
  let open Websocket in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system
  >>= fun endp ->
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  Conduit_lwt_unix.endp_to_client ~ctx endp
  >>= fun client ->
  connect ~ctx client uri
  >>= fun conn ->
  let close_sent = ref false in
  let rec react () =
    Websocket_lwt_unix.read conn
    >>= function
    | {Frame.opcode= Ping; _} ->
        write conn (Frame.create ~opcode:Pong ()) >>= react
    | {opcode= Close; content; _} ->
        (* Immediately echo and pass this last message to the user *)
        ( if !close_sent then
            Lwt.return_unit
          else if String.length content >= 2 then
            write conn
              (Frame.create ~opcode:Close ~content:(String.sub content 0 2) ())
          else write conn (Frame.close 1000) )
        >>= fun () ->
        Websocket_lwt_unix.close_transport conn
    | {opcode= Pong; _} -> react ()
    | {opcode= Text; content; _} | {opcode= Binary; content; _} ->
        handle_in_content conn content >>= react
    | _ -> Websocket_lwt_unix.close_transport conn in
  let rec pushf () =
    Lwt_io.(read_line_opt stdin)
    >>= function
    | None ->
        Lwt_log.debug ~section "Got EOF. Sending a close frame."
        >>= fun () -> write conn (Frame.create ~opcode:Close ())
        >>= fun () -> close_sent := true; pushf ()
    | Some content -> write conn (Frame.create ~content ()) >>= pushf in
  pushf () <?> react ()

let rec react client client_id =
  let open Websocket in
  Connected_client.recv client
  >>= fun fr ->
  Lwt_log.debug_f ~section "Client %d: %S" client_id Frame.(show fr)
  >>= fun () ->
  match fr.opcode with
  | Frame.Opcode.Ping ->
      Connected_client.send client
        Frame.(create ~opcode:Opcode.Pong ~content:fr.content ())
      >>= fun () -> react client client_id
  | Close ->
      (* Immediately echo and pass this last message to the user *)
      if String.length fr.content >= 2 then
        let content = String.sub fr.content 0 2 in
        Connected_client.send client
          Frame.(create ~opcode:Opcode.Close ~content ())
      else Connected_client.send client @@ Frame.close 1000
  | Pong -> react client client_id
  | Text | Binary ->
      Connected_client.send client fr >>= fun () -> react client client_id
  | _ -> Connected_client.send client Frame.(close 1002)

let apply_loglevel = function
  | 2 -> Lwt_log.(add_rule "*" Info)
  | 3 -> Lwt_log.(add_rule "*" Debug)
  | _ -> ()

let () =
  let uri = ref "" in
  let speclist =
    Arg.align
      [ ("-loglevel", Arg.Int apply_loglevel, "1-3 Set loglevel") ] in
  let anon_fun s = uri := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg ;
  Lwt_main.run (client (Uri.of_string !uri))
