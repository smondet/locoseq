(**************************************************************************)
(*  Copyright (c) 2007, Sebastien MONDET                                  *)
(*                                                                        *)
(*  Permission is hereby granted, free of charge, to any person           *)
(*  obtaining a copy of this software and associated documentation        *)
(*  files (the "Software"), to deal in the Software without               *)
(*  restriction, including without limitation the rights to use,          *)
(*  copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*  copies of the Software, and to permit persons to whom the             *)
(*  Software is furnished to do so, subject to the following              *)
(*  conditions:                                                           *)
(*                                                                        *)
(*  The above copyright notice and this permission notice shall be        *)
(*  included in all copies or substantial portions of the Software.       *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*  OTHER DEALINGS IN THE SOFTWARE.                                       *)
(**************************************************************************)


(******************************************************************************)
(** Test functions **)

(* MIDI file parsing *)

let test_parse_and_print_midi_file file =
  try (
    let midi_data = MidiFile.parse_smf file in
    (*
      let the_other_file = "copy_of_" ^ file in
      MidiFile.write_smf midi_data the_other_file ;
      let midi_data = MidiFile.parse_smf the_other_file in
    *)
    Printf.printf "File %s\n" file;
    Printf.printf "%s" (Midi.midi_to_string midi_data) ;
  ) with exc -> Printf.printf "Exception: %s\n" (Printexc.to_string exc);
;;

(* Inspection functions *)

module Seq = AlsaSequencer 
module Tim = AlsaSequencer

let used_channel = ref stdout
let pr (form:('a, out_channel, unit ) format) = (
  Printf.fprintf !used_channel ( form ^^ "%!" )
)

let str_of_cmd cmd = (

  let uname_in = Unix.open_process_in cmd in
  let buf = Buffer.create 128 in
  let _ = try (
    while true do
      Buffer.add_char buf (input_char uname_in);
    done
  ) with End_of_file -> () in
  let _ = Unix.close_process_in uname_in in
  Buffer.contents buf 
)
let print_cmd cmd = (
  pr "```\nocaml@home:/bin/sh$ %s\n%s```\n" cmd (str_of_cmd cmd);
)

let test_timer info = (

  let is_end = ref false in
  while not !is_end do
    let _ = 
      try (
        let my_timer = Tim.make_timer info in
        Tim.set_ticks my_timer 4 ;
        Tim.start_timer my_timer ;
        let status = Tim.get_status my_timer in
        pr ": Status:\n  ``resolution: %d, lost: %d, overrun: %d, queue:%d``\n\n"
        status.Tim.resolution
        status.Tim.lost
        status.Tim.overrun
        status.Tim.queue ;
        pr ": Trying the wait next:\n";
        let count = Tim.wait_next_tick my_timer 500 in
        if count = -1 then (
          pr "- Timer wait_next_tick has returned -1. -> **TIME OUT**\n" 
        ) else (
          pr "- **Succeeded**: count = %d\n" count ;
        );
        Tim.stop_timer my_timer ;
        pr "- timer stopped\n"
      ) with exn -> (
        pr "\n**Got an exception**: %s\n" (Printexc.to_string exn)
      );
    in
    pr "\n\n\n%!" ;
    if (info.Tim.t_card <> 0 ) then (
      pr "\n**Retrying with card = 0 (a.k.a //The Ugly Hack//)**\n";
      info.Tim.t_card <- 0 ;
    ) else (
      is_end := true ;
    );

  done;

)


let make_inspection file = (

  let f = open_out file in
  used_channel := f ;

  pr "%s's Portability Inspection Report\n" StringServer.app_name;
  pr "Generated by %s %s\n" StringServer.app_name (StringServer.version ());
  pr "%s\n\n\n" (Log.rfc_822_date ()) ;


  pr "= Environnement =\n\n" ;
  pr "== System ==\n\n" ;
  print_cmd "uname -a";
  print_cmd "cat /proc/sys/dev/rtc/max-user-freq";
  print_cmd "cat /proc/driver/rtc";
  print_cmd "cat /proc/interrupts";
  print_cmd "cat /proc/asound/version" ;
  print_cmd "cat /proc/asound/timers" ;
  print_cmd "cat /proc/asound/modules" ;
  print_cmd "cat /proc/asound/devices" ;
  print_cmd "cat /proc/asound/seq/drivers" ;
  print_cmd "cat /proc/asound/seq/clients" ;
  print_cmd "ls -l /dev/rtc /dev/snd/seq /dev/snd/timer";
  print_cmd "groups";

  pr "== Ocaml Data ==\n\n" ;
  pr  "| Sys.max_array_length    | %d |\n" Sys.max_array_length ;
  pr  "| Sys.max_string_length   | %d |\n" Sys.max_string_length;
  pr  "| Sys.ocaml_version       | %s |\n" Sys.ocaml_version;
  pr  "| Sys.os_type             | %s |\n" Sys.os_type;
  pr  "| Sys.word_size           | %d |\n" Sys.word_size;

  pr "\n== Libraries ==\n\n" ;
  let x,y,z = GMain.Main.version in
  pr "- GTK version: ``%d.%d.%d``\n\n" x y z ;
  pr  "- GtkInit.locale:\n```\n%s\n```\n" (
    try 
      while true do
        let i = String.index GtkInit.locale ';' in
        GtkInit.locale.[i] <- '\n' ;
      done;
      GtkInit.locale (* never used !*)
    with e -> GtkInit.locale
  );

  let l = Tim.query_info () in
  pr "\n\n= Inspection of the TIMERS =\n\n" ;
  List.iter (
    fun info ->
      let cla = info.Tim.t_class in
      let scl = info.Tim.t_sclass in
      let crd = info.Tim.t_card in
      let dev = info.Tim.t_device in
      let sdv = info.Tim.t_subdevice in 
      pr "== Timer (%d, %d, %d, %d, %d) ==\n\n: Information:\n"
      cla scl crd dev sdv;
      pr "  CLASS=%d (%s),\nSCLASS=%d (%s),\nCARD=%d, DEV=%d, SUBDEV=%d\n\n"
      info.Tim.t_class     (Tim.timer_class_to_string       info.Tim.t_class)
      info.Tim.t_sclass    (Tim.timer_slave_class_to_string info.Tim.t_sclass)
      info.Tim.t_card     info.Tim.t_device    info.Tim.t_subdevice    ;
      (* pr "\n\n=== Test ===\n\n%!" ; *)

      test_timer info ;
      pr "\n\n\n%!" ;

  ) l;

  pr "= Alsa interface =\n\n";
  let seq = Seq.make_sequencer "da_dummy_alsa_client" [| "one_in" |] [| "one_out" |] in
  let info = Seq.get_queue_timer_info seq in

  let cla = info.Tim.t_class in
  let scl = info.Tim.t_sclass in
  let crd = info.Tim.t_card in
  let dev = info.Tim.t_device in
  let sdv = info.Tim.t_subdevice in 
  pr "== Queue Timer (%d, %d, %d, %d, %d) ==\n\n: Information:\n\n"
  cla scl crd dev sdv;
  pr "- CLASS=%d (%s),\nSCLASS=%d (%s),\nCARD=%d, DEV=%d, SUBDEV=%d\n\n"
  info.Tim.t_class     (Tim.timer_class_to_string       info.Tim.t_class)
  info.Tim.t_sclass    (Tim.timer_slave_class_to_string info.Tim.t_sclass)
  info.Tim.t_card     info.Tim.t_device    info.Tim.t_subdevice    ;

  test_timer info ;
)


(******************************************************************************)
(** Command line functions **)

let short_usage = "Arguments : -gui | -inspect <report file path> | -parse <MIDI file> [<MIDI file> ...]" ;;

let gui = ref false ;;

let options = [
  ("-gui", Arg.Set gui, "Launch GUI");
  ("-parse", Arg.Rest test_parse_and_print_midi_file, "Test, Parse and print MIDI file");
  ("-inspect", Arg.String make_inspection, "Write inspection to file")
]
;;

let ignore_unknown_arguments str = 
  Printf.printf "%s: argument '%s' ignored.\n" Sys.argv.(0) str;
;;


(******************************************************************************)
(** Main **)

let main () =
  Arg.parse options ignore_unknown_arguments short_usage;
  if 1 >= !Arg.current 
  then (
    Arg.usage options short_usage;
    exit 1;
  );

  if !gui then Gui.start () ;

;;

let _ =
  Printexc.print main () ;
;;

