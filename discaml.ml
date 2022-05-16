
open Lwt
open Ppx_lwt
open Cohttp
open Cohttp_lwt_unix

type snowflake = int64

let api_url = "https://discord.com/api"

module Util = struct
    let user_agent = "DiscordBot (foo, 1)"

    let uri_of_resource res = Uri.of_string (api_url ^ res)

    let header_of_token token =
        ("Authorization", "Bot " ^ token)

    let basic_header token =
        [header_of_token token; ("User-Agent", user_agent)]

    let http_get ?(headers = []) token res =
        let coheaders = Header.of_list (headers @ (basic_header token)) in
        let%lwt (resp, body) = Client.get ~headers:coheaders (uri_of_resource res) in
        body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string

    let http_patch ?(headers = []) token res json_body =
        let coheaders = Header.of_list (headers @ (basic_header token)) in
        let out_body = Cohttp_lwt.Body.of_string (Yojson.Safe.to_string json_body) in
        let%lwt (resp, body) = Client.patch ~headers:coheaders ~body:out_body (uri_of_resource res) in
        body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string

    let http_post ?(headers = []) token res json_body =
        let coheaders = Header.of_list (headers @ (basic_header token)) in
        let out_body = Cohttp_lwt.Body.of_string (Yojson.Safe.to_string json_body) in
        let%lwt (resp, body) = Client.post ~headers:coheaders ~body:out_body (uri_of_resource res) in
        body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string

    let http_delete ?(headers = []) token res =
        let coheaders = Header.of_list (headers @ (basic_header token)) in
        let%lwt (resp, body) = Client.delete ~headers:coheaders (uri_of_resource res) in
        body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string
end

module User = struct
    let get_current_user token =
        Util.http_get token "/users/@me"

    let get_user token id =
        Util.http_get token ("/users/" ^ id)

    let modify_current_user token json =
        let ctype_hdr = ("Content-Type", "application/json") in
        Util.http_patch ~headers:[ctype_hdr] token "/users/@me" json

    let get_current_user_guilds token =
        Util.http_get token "/users/@me/guilds"

    let get_current_user_guild_member token id =
        Util.http_get token ("/users/@me/guilds" ^ id ^ "/member")

    let leave_guild token id =
        Util.http_delete token ("/users/@me/guilds/" ^ id)

    let create_dm token json =
        let ctype_hdr = ("Content-Type", "application/json") in
        Util.http_post ~headers:[ctype_hdr] token "/users/@me/channels" json

    let create_group_dm token json =
        let ctype_hdr = ("Content-Type", "application/json") in
        Util.http_post ~headers:[ctype_hdr] token "/users/@me/channels" json

    let get_user_connections token =
        Util.http_get token "/users/@me/connections"
end

module Channel = struct
    (*
    let get_channel token id =
        Util.http_get token ("/channels/" ^ id)

    let modify_channel token id json =
        Util.http_patch token ("/channels/" ^ id) json
    *)

    (* Types would take a lot to implement, so how about we stick to handling
     * JSON directly and/or converting on the fly for now
    type thread_metadata =
        {
            archived : bool;
            auto_archive_duration : int;
            archive_timestamp : Timedesc.timestamp;
            locked : bool;
            invitable : bool option;
            create_timestamp : Timedesc.timestamp option
        }

    type thread_member =
        {
            id : snowflake option;
            user_id : snowflake option;
            join_timestamp : Timedesc.timestamp;
            flags : int
        }

    type channel =
        {
            id : snowflake;
            type' : int;
            guild_id : snowflake option;
            position : int option;
            permission_overwrites : int option list;
            (* 1-100 chars *)
            name : string option;
            (* 0-1024 chars *)
            topic : string option;
            nsfw : bool option;
            last_message_id : snowflake option;
            bitrate : int option;
            user_limit : int option;
            rate_limit_per_user : int option;
            recipients : User.user option;
            icon : string option;
            owner_id : snowflake option;
            application_id : snowflake option;
            parent_id : snowflake option;
            last_pin_timestamp : Timedesc.timestamp option;
            rtc_region : string option;
            video_quality_mode : int option;
            message_count : int option;
            member_count : int option;
            thread_metadata : thread_metadata option;
            member : thread_member option;
            default_auto_archive_duration : int option;
            permissions : string option;
            flags : int option
        }*)
end

module Gateway = struct
end
