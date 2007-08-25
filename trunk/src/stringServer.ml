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

let spr = Printf.sprintf
let soi = string_of_int

let app_name = "LoCoSEQ"

let version () = "0.0.1" 

let alsa_client_name () = "locoseq"

let out_put_ports = [|
  (alsa_client_name ()) ^": out " ^ (spr "%2d"   0) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"   1) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"   2) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"   3) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"   4) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"   5) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"   6) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"   7) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"   8) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"   9) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"  10) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"  11) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"  12) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"  13) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"  14) ;
  (alsa_client_name ()) ^": out " ^ (spr "%2d"  15) ;
|] 
let in_put_ports = [|
  (alsa_client_name ()) ^": in"
|]

let gui_quarters () = "quarters"
let gui_44_bars () = "4/4 bars"
let gui_ticks () = "ticks"


let midi_note_on = "MidiNoteOn"
let key_code = "Key"
let velo_to_bpm = "Set_Velocity_As_BPM"
let toggle_track = "Toggle_Track"


let playing_and_schedstop = "p"
let playing_and_schedstop_color = "#FF0000"
let playing = "P"
let playing_color = "#00FF00"

let status_color_of_bools (playing,sched_play,sched_stop) =
  match (playing,sched_play,sched_stop) with
  | false,false,_     -> "Stop","#FFFFFF"
  | true ,_    ,false -> "Play","#00FF00"
  | false,true ,_     -> "S->P","#00AA00"
  | true ,_    ,true  -> "P->S","#00BB00"

let selected_color = "#2233EE"
let normal_color   = "#111111"


let unitize_length lgth pqn = (
  if (lgth mod (4*pqn)) = 0 then (
    (lgth / (4*pqn) , 2)
  ) else if (lgth mod pqn) = 0
  then (lgth / pqn , 1)
  else lgth, 0
) 
let unitize_length_tuple lgth pqn = (
  lgth / (4*pqn) ,
  (lgth mod (4*pqn)) / pqn ,
  (lgth mod pqn) 
)
let string_of_length lgth pqn = (
  let b,q,t = unitize_length_tuple lgth pqn in
  spr "%dB:%dQ:%dT" b q t 
)
(* if (lgth mod pqn) = 0 then ( *)
(* (string_of_int (lgth / pqn)) ^ "Q") else ( (string_of_int lgth) ^ "T")  *)

let string_of_meta_event event pqn = (
  match event with
  | `track_set_on  (t,id) -> spr "[%s] Track %d On"  (string_of_length t pqn) id
  | `track_set_off (t,id) -> spr "[%s] Track %d Off" (string_of_length t pqn) id
  | `set_bpm       (t,b ) -> spr "[%s] Set BPM = %d" (string_of_length t pqn)  b
  | `track_on    (b,e,id) ->
      spr "[%s - %s] Keep Track [%d] ON "
      (string_of_length b pqn) (string_of_length e pqn) id
)


let make_appwin_title songname =
    app_name ^ " " ^ (version ()) ^(
    match songname with
    | "" -> "   -- (no name song) --"
    | _  -> "    [" ^ songname ^ "]" 
  )


let global_available_keys = [|
  "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
  "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z";
  "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M";
  "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z";
|]
let key_to_int str = (
  let res = ref 0 in
  let finish = ref false in
  (* We "use" the out of bounds exception *)
  while (not !finish) do
    if (str = global_available_keys.(!res)) then (
      finish := true ;
    ) else (
      incr res ;
    );
  done;
  !res
)
let int_to_key i = (
  (* Log.p "int_to_key %d\n" i ; *)
  global_available_keys.(i)
)

let str_of_midi_arg arg =
  if (arg >= 0  ) then (
    spr "%3d" arg
  ) else (
    "___"
  )

let iact_to_string (inp,(cmd,arg)) =
  let inp_str =
    match inp with
    | `midi_event (stat,chan,note,velo) -> 
        spr "midi event (%s:%s:%s:%s)" 
        (str_of_midi_arg stat)
        (str_of_midi_arg chan)
        (str_of_midi_arg note)
        (str_of_midi_arg velo)
    | `custom c -> spr "keyboard '%s'" (int_to_key c)
  in
  let argument = 
    match arg with
    | `direct_int v       ->  (soi v)
    | `midi_status    -> "Midi-Status Value"
    | `midi_channel   -> "Midi-Channel Value"
    | `midi_note      -> "Midi-Note Value"
    | `midi_velocity  -> "Midi-Velocity Value"
  in
  let cmd_str =
    match cmd with
    | `set_BPM                 ->  "Set BPM to " ^   argument
    | `incr_BPM                ->  "Increase BPM of " ^   argument
    | `decr_BPM                ->  "Decrease BPM of " ^   argument
    | `toggle_track            ->  "Toggle Track " ^ argument
    | `track_on                ->  "Track " ^   argument ^ " ON"
    | `track_off               ->  "Track" ^   argument ^ " OFF"
    | `schedule_toggle_track   ->  "Schedule Toggle for Trk. " ^ argument
    | `schedule_track_on       ->  "Schedule TrackON for Trk. " ^ argument
    | `schedule_track_off      ->  "Schedule TrackOFF for Trk. " ^ argument
    | `play                    ->  "Play"               
    | `stop                    ->  "Stop"                   
    | `mute_all                ->  "Mute All Tracks"                          
  in 
  (spr "On [%s]: %s" inp_str cmd_str )


let keyboard = "keyboard"
let midi_evt = "midi event"

let create_new_handler = "Create your <b>new</b> input handler"
let edit_handler m = ("Edit the handler: <b>" ^ m ^ "</b>")


let global_available_midi_events = [
  "" ;
  "NoteOFF           (0x80)" ; 
  "NoteON            (0x90)" ; 
  "AfterTouch        (0xA0)" ; 
  "ControlChange     (0xB0)" ; 
  "ProgramChange     (0xC0)" ; 
  "ChannelAfterTouch (0xD0)" ; 
  "PitchRange        (0xE0)" ; 
]
let midi_status_of_string str = 
  match str with
  | ""                         -> -1
  | "NoteOFF           (0x80)" -> 0x80 
  | "NoteON            (0x90)" -> 0x90 
  | "AfterTouch        (0xA0)" -> 0xA0 
  | "ControlChange     (0xB0)" -> 0xB0 
  | "ProgramChange     (0xC0)" -> 0xC0 
  | "ChannelAfterTouch (0xD0)" -> 0xD0 
  | "PitchRange        (0xE0)" -> 0xE0 
  | _ -> failwith "Unrecognizable midi event !!" 

let int_of_midi_status ms = (
  match ms with
  | -1   -> 0
  | 0x80 -> 1
  | 0x90 -> 2
  | 0xA0 -> 3
  | 0xB0 -> 4
  | 0xC0 -> 5
  | 0xD0 -> 6
  | 0xE0 -> 7
  | _ -> failwith ("Unrecognizable midi event: " ^ (soi ms))
)

let midi_channel_strings = [
  ""; "0"; "1"; "2"; "2"; "4"; "5"; "6"; "7"; "8"; "9";
  "10"; "11"; "12"; "13"; "14"; "15";
]
let midi_channel_of_string str =
  match str with
  | None | Some "" -> -1
  | Some nb -> int_of_string nb

let action_strings = [
  "set_BPM               ";
  "incr_BPM              ";
  "decr_BPM              ";
  "toggle_track          ";
  "track_on              ";
  "track_off             ";
  "schedule_toggle_track ";
  "schedule_track_on     ";
  "schedule_track_off    ";
  "play                  ";
  "stop                  ";
  "mute_all              ";
]
let action_of_string str =
  match str with
  | "set_BPM               " -> `set_BPM              
  | "incr_BPM              " -> `incr_BPM             
  | "decr_BPM              " -> `decr_BPM             
  | "toggle_track          " -> `toggle_track         
  | "track_on              " -> `track_on             
  | "track_off             " -> `track_off            
  | "schedule_toggle_track " -> `schedule_toggle_track
  | "schedule_track_on     " -> `schedule_track_on    
  | "schedule_track_off    " -> `schedule_track_off   
  | "play                  " -> `play                 
  | "stop                  " -> `stop                 
  | "mute_all              " -> `mute_all             
  | _ -> failwith "Unrecognizable action !!" 

let argument_string_list = [
  "direct_int   "  ; 
  "midi_status  "  ;  
  "midi_channel "  ;  
  "midi_note    "  ;
  "midi_velocity"  ;   
]
let arg_spec_of_string str i =
  match str with
  | s when s  =  "direct_int   "  -> (`direct_int i  )
  | s when s  =  "midi_status  "  -> (`midi_status   ) 
  | s when s  =  "midi_channel "  -> (`midi_channel  ) 
  | s when s  =  "midi_note    "  -> (`midi_note     )
  | s when s  =  "midi_velocity"  -> (`midi_velocity )  
  | _ -> failwith "Unrecognizable arg_spec string"


let note_names = [|
  "C"; "C#"; "D"; "D#"; 
  "E"; "F"; "F#"; "G";
  "G#"; "A"; "A#"; "B"
|]



let err_choose_input_type =  "Try again: <b>You must choose a type of input event</b>"
let err_choose_action_type = "Try again: <b>You must choose a type of action</b>"
let err_choose_argument_type = "Try again: <b>You must choose another type of argument</b>"
let err_keyboard_vs_midiarg = "Try again: <b>You can't choose a midi argument with a keyboard input"


let notsaved_title = "The song is not saved"

let save_before_quit_msg = "There seems to be some not saved changes, do you want to save before quitting ?"
let save_before_quit_save = "Save & Quit"
let save_before_quit_dont = "Don't Save & Quit"
let save_before_quit_cancel = "Don't Quit"


let save_before_open_msg = "There seems to be some not saved changes, do you want to save before openning another song ?"
let save_before_open_save = "Save & Continue"
let save_before_open_dont = "Don't Save & Continue"
let save_before_open_cancel = "Don't Continue"

let save_before_new_msg = "There seems to be some not saved changes, do you want to save before starting another song ?"
let save_before_new_save = "Save & Clear"
let save_before_new_dont = "Don't Save & Clear"
let save_before_new_cancel = "Don't Clear"






