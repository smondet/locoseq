



(******************************************************************************)
(*
 * Logging:
 *
 *
 *
 * *)

module P = Printf ;;

let used_channel = ref stdout ;;
let iam_logging = ref true ;;

let p (form:('a, out_channel, unit ) format) =
  (* let prefix = format_of_string ( *)
    (* P.sprintf "[LOG:] " ) in *)
  (* let suffix = format_of_string "%!" in *)
  P.fprintf !used_channel ( "[LOG:] " ^^ form ^^ "%!" ) 
;;

let rfc_822_date () = (

  let module U = Unix in
  let t = U.time () in
  let st = U.localtime t in
  let sec   = st.U.tm_sec in (* : int;*)
  let min   = st.U.tm_min in (* : int;*)
  let hour  = st.U.tm_hour in (* : int;*)
  let mday  = st.U.tm_mday in (* : int;*)
  let mon   = st.U.tm_mon in (* : int;*)
  let year  = st.U.tm_year in (* : int;*)
  let wday  = st.U.tm_wday in (* : int;*)
  let yday  = st.U.tm_yday in (* : int;
  let isdst = st.U.tm_isdst in (* : bool;*)
*)
  let diff = 
    if yday =(U.gmtime t).U.tm_yday then
      hour - (U.gmtime t).U.tm_hour 
    else
      if yday > (U.gmtime t).U.tm_yday then
        (24 + hour) - (U.gmtime t).U.tm_hour 
    else
        hour - (24 + (U.gmtime t).U.tm_hour )
  in
  let sdiff =
    if diff >= 0 then
      Printf.sprintf "+%02d00" diff
    else
      Printf.sprintf "-%02d00" (- diff)
  in
  let dday = 
    match wday with
    | 0 -> "Sun" 
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | _ -> "???"
  in
  let mmon = 
    match mon with
    | 0  -> "Jan"
    | 1  -> "Feb"
    | 2  -> "Mar"
    | 3  -> "Apr"
    | 4  -> "May"
    | 5  -> "Jun"
    | 6  -> "Jul"
    | 7  -> "Aug"
    | 8  -> "Sep"
    | 9  -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> "???"
  in
  let s =
    Printf.sprintf
    "%s, %d %s %d  %d:%02d:%02d  %s"
    dday mday mmon (1900 + year) hour min sec sdiff in
    (* yday  *)
    (* isdst *)
  s

)

