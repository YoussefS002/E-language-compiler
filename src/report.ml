open Options
open Utils

type html_node =
  | Img of string
  | Code of string
  | Paragraph of string
  | List of html_node list

let rec print_html oc = function
    Img s -> Printf.fprintf oc "<img src=\"%s\" />\n" s
  | Code s -> Printf.fprintf oc "<pre>%s</pre>\n" s
  | Paragraph s -> Printf.fprintf oc "<div>%s</div>\n" s
  | List l -> List.iter (print_html oc) l

type report_section = { sect_title: string;
                        sect_id: string;
                        sect_content: html_node
                      }

let report = ref ([]: report_section list)

let add_to_report id title content =
  report := !report @ [{ sect_id = id; sect_title = title; sect_content = content }]

let make_report filename report () =
  let html = open_out (filename ^ ".html") in
  Printf.fprintf html "\
<html>\n\
    <head>\n\
            <link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\">\n\
    <script src=\"https://kit.fontawesome.com/1f5d81749b.js\" crossorigin=\"anonymous\"></script>\n\
    <style type=\"text/css\">\n\
    a.anchor {\n\
      display: block; \
      position: relative; \
      left: -250px; \
    visibility: hidden;\
    }\n\
    </style>\n\
    </head>\n\
    <body>\n\
";
  Printf.fprintf html "<div \
                       class=\"w3-container w3-cell\" \
                       style=\"position: fixed; z-index: 1; top: 0; bottom: 0; width: 250px; overflow-y: scroll;\"\
                       >\n  <ul class=\"w3-ul\">\n";
  let t = Unix.time () in
  let tm = Unix.localtime t in
  let open Unix in
  Printf.fprintf html "<li>%02d/%02d/%04d - %02dh%02d</li>"
    tm.tm_mday
    (tm.tm_mon + 1)
    (tm.tm_year + 1900)
    tm.tm_hour
    tm.tm_min
  ;
  Printf.fprintf html "  <li><a href=\"../results.html\"><i class=\"fa fa-home\"></i> Results</a></li>\n";
  List.iter
    (fun { sect_id; sect_title; _ } ->
       Printf.fprintf html "  <li><a href=\"#%s\">%s</a></li>\n" sect_id sect_title
    )
    !report;
  Printf.fprintf html "</ul></div><div \
                       class=\"w3-container w3-cell-row\" \
                       style=\"margin-left: 250px;\"\
                       ><a class=\"anchor\" id=\"top\"></a>";
  List.iter
    (fun { sect_id; sect_title; sect_content } ->
       Printf.fprintf html "<fieldset>\n\
                            <a class=\"anchor\" id=\"%s\"></a>\n\
                            <h3><a href=\"#top\">&uarr;</a> %s</h3>\n\
                            %a\n\
                            </fieldset>\n" sect_id sect_title print_html sect_content
    )
    !report;
  Printf.fprintf html "\
</div>\n\
</body>\n\
</html>";
  close_out html;
  ()

let call_dot report_sectid report_secttitle file () : unit =
  if not !Options.no_dot
  then begin
    let r = Sys.command (Format.sprintf "dot -Tsvg %s -o %s.svg" file file) in
    add_to_report report_sectid report_secttitle (Img (Filename.basename file^".svg"));
    ignore r
  end

(*  *)


type run_result = {
  step: string;
  retval: int option;
  output: string;
  error: string option;
  time: float;
}

type compile_result = {
  step: string;
  error: string option;
  data: Yojson.t
}

type result = RunRes of run_result
            | CompRes of compile_result


let results : result list ref = ref []


let record_compile_result ?error:(error=None) ?data:(data=[]) step =
  let data = if not !Options.nostats then `List data else `Null in
  results := !results @ [CompRes { step; error; data}]


let kill pid sign =
  try Unix.kill pid sign with
  | Unix.Unix_error (e,f,p) ->
    begin match e with
      | ESRCH -> ()
      | _ -> Printf.printf "%s\n" ((Unix.error_message e)^"|"^f^"|"^p)
    end
  | e -> raise e

let run_exn_to_error f x =
  try f x with
  | e -> Error (Printexc.to_string e)

let timeout (f: 'a -> 'b res) (arg: 'a) (time: float) : ('b * string) res =
  let pipe_r,pipe_w = Unix.pipe () in
  (match Unix.fork () with
   | 0 ->
     let r =
       run_exn_to_error f arg >>= fun v ->
       OK (v, Format.flush_str_formatter ()) in
     let oc = Unix.out_channel_of_descr pipe_w in
     Marshal.to_channel oc r [];
     close_out oc;
     exit 0
   | pid0 ->
     (match Unix.fork () with
      | 0 -> Unix.sleepf time;
        kill pid0 Sys.sigkill;
        let oc = Unix.out_channel_of_descr pipe_w in
        Marshal.to_channel oc (Error (Printf.sprintf "Timeout after %f seconds." time)) [];
        close_out oc;
        exit 0
      | _ -> let ic = Unix.in_channel_of_descr pipe_r in
        let result = Marshal.from_channel ic in
        result ))

let run step flag eval p =
  if flag then begin
    let starttime = Unix.gettimeofday () in
    let res = timeout
        (fun (p, params) -> eval Format.str_formatter p !heapsize params)
        (p, !params)
        !Options.timeout in
    let timerun = Unix.gettimeofday () -. starttime in
    let rres = { step ; retval = None; output=""; error = None; time = timerun} in
    let rres =
    begin match res with
      | OK (v, output) ->  { rres with retval = v; output }
      | Error msg -> { rres with error = Some msg }
    end in
    results := !results @ [RunRes rres];
    add_to_report step ("Run " ^ step) (
      Paragraph
        (
          Printf.sprintf "With parameters : [%s]<br>\n" (String.concat"," (List.map string_of_int !params))
          ^ Printf.sprintf "Mem size : %d bytes<br>\n" !heapsize
          ^ Printf.sprintf "Return value : %s<br>\n" (match rres.retval with | Some v -> string_of_int v | _ -> "none")
          ^ Printf.sprintf "Output : <pre style=\"padding: 1em; background-color: #ccc;\">%s</pre>\n" rres.output
          ^
          (match rres.error with
           | Some msg -> Printf.sprintf "Error : <pre style=\"padding: 1em; background-color: #fcc;\">\n%s</pre>\n" msg
           | _ -> "")
          ^ Printf.sprintf "Time : %f seconds<br>\n" timerun
        )
    )
  end


let json_output_string () =
  let jstring_of_ostring o =
    match o with
    | None -> `Null
    | Some s -> `String s
  in
  let j = `List (List.map (function
      | RunRes { step; retval; output; error; time } ->
        `Assoc [("runstep",`String step);
                ("retval", match retval with Some r -> `Int r | None -> `Null);
                ("output", `String output);
                ("error", jstring_of_ostring error);
                ("time", `Float time)
               ]
      | CompRes { step; error; data } ->
        `Assoc [("compstep",`String step);
                ("error", jstring_of_ostring error);
                ("data", data)
               ]
    ) !results) in
  (Yojson.pretty_to_string j)
