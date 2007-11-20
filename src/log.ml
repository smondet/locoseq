(******************************************************************************)
(*      Copyright (c) 2007, Sebastien MONDET                                  *)
(*                                                                            *)
(*      Permission is hereby granted, free of charge, to any person           *)
(*      obtaining a copy of this software and associated documentation        *)
(*      files (the "Software"), to deal in the Software without               *)
(*      restriction, including without limitation the rights to use,          *)
(*      copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*      copies of the Software, and to permit persons to whom the             *)
(*      Software is furnished to do so, subject to the following              *)
(*      conditions:                                                           *)
(*                                                                            *)
(*      The above copyright notice and this permission notice shall be        *)
(*      included in all copies or substantial portions of the Software.       *)
(*                                                                            *)
(*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*      OTHER DEALINGS IN THE SOFTWARE.                                       *)
(******************************************************************************)



(**
Some utilities for logging.

@author S.Mondet
*)



(** One module providing color in the ANSI terminal *)
module Color = struct

  (** The head of a colored string *)
  let before str = "\027[;" ^ str ^ "m"

  (** The tail of a colored string *)
  let after = "\027[0m"

  (** Colorize a string *)
  let color_str color str = (before color) ^ str ^ after

  (** Green foreground *)
  let fg_green str = color_str "32" str

  (** Bold string *)
  let bold str = color_str "1" str

  (** Red foreground *)
  let fgred str = color_str "31" str

  (** Bold & Red foreground *)
  let fg_bold_red str = color_str "31;1" str

end


(** The channel used for logging (you can set it to a file) *)
let used_channel = ref stdout


(**
  The most-used debug, works like  Printf.printf (but you can choose a file for
  output)
  *)
let p (form:('a, out_channel, unit ) format) = (
  Printf.fprintf !used_channel ( "[DEBUG:] " ^^ form ^^ "%!" ) 
)


(** The more "official" log *)
let log (form:('a, out_channel, unit ) format) = (
  let head_fmt = Scanf.sscanf_format ("[" ^ (Color.bold "LOG") ^ ":] ") "" in
    Printf.fprintf !used_channel ( head_fmt ^^ form ^^ "%!" ) 
)


(** The warning *)
let warn (form:('a, out_channel, unit ) format) = (
  let head =
    Scanf.sscanf_format ("[" ^ (Color.fg_bold_red "WARNING") ^ ":] ") ""
  in
    Printf.fprintf !used_channel ( head ^^ form ^^ "%!" ) 
)




(** Get a string containing date in RFC 822 format
  (e.g. used for e-mails)
  *)
let rfc_822_date () = (

  let module U = Unix in
  let t = U.time () in
  let st = U.localtime t in
  let sec   = st.U.tm_sec in 
  let min   = st.U.tm_min in 
  let hour  = st.U.tm_hour in
  let mday  = st.U.tm_mday in
  let mon   = st.U.tm_mon in 
  let year  = st.U.tm_year in
  let wday  = st.U.tm_wday in
  let yday  = st.U.tm_yday in
  (* let isdst = st.U.tm_isdst in (* : bool;*)*)

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
      | 0 -> "Sun" | 1 -> "Mon" | 2 -> "Tue" | 3 -> "Wed"
      | 4 -> "Thu" | 5 -> "Fri" | 6 -> "Sat" | _ -> "???"
  in
  let mmon = 
    match mon with
      | 0  -> "Jan" | 1  -> "Feb" | 2  -> "Mar" | 3  -> "Apr"
      | 4  -> "May" | 5  -> "Jun" | 6  -> "Jul" | 7  -> "Aug"
      | 8  -> "Sep" | 9  -> "Oct" | 10 -> "Nov" | 11 -> "Dec"
      | _ -> warn "Unknown month in Log.rfc_822_date: %d\n" mon ; "???"
  in
  let s =
    Printf.sprintf "%s, %d %s %d  %d:%02d:%02d  %s"
      dday mday mmon (1900 + year) hour min sec sdiff
  in
    (* yday  *)
    (* isdst *)
    s
)

