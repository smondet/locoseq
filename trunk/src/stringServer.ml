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
StringServer provides all the {i user visible} strings and some functions to
manipulate them.
*)


let spr = Printf.sprintf
let soi = string_of_int

(** Strings related to the {i application} (name, version, client...) *)
module App =  struct

  let app_name = ref "LoCoSEQ"

  let version = ref "0.0.1" 

  let jack_client_name = ref "locoseq"

  let out_put_ports = [|
    !jack_client_name ^":out_" ^ (spr "%02d"   0) ;
    !jack_client_name ^":out_" ^ (spr "%02d"   1) ;
    !jack_client_name ^":out_" ^ (spr "%02d"   2) ;
    !jack_client_name ^":out_" ^ (spr "%02d"   3) ;
    !jack_client_name ^":out_" ^ (spr "%02d"   4) ;
    !jack_client_name ^":out_" ^ (spr "%02d"   5) ;
    !jack_client_name ^":out_" ^ (spr "%02d"   6) ;
    !jack_client_name ^":out_" ^ (spr "%02d"   7) ;
    !jack_client_name ^":out_" ^ (spr "%02d"   8) ;
    !jack_client_name ^":out_" ^ (spr "%02d"   9) ;
    !jack_client_name ^":out_" ^ (spr "%02d"  10) ;
    !jack_client_name ^":out_" ^ (spr "%02d"  11) ;
    !jack_client_name ^":out_" ^ (spr "%02d"  12) ;
    !jack_client_name ^":out_" ^ (spr "%02d"  13) ;
    !jack_client_name ^":out_" ^ (spr "%02d"  14) ;
    !jack_client_name ^":out_" ^ (spr "%02d"  15) ;
  |] 

  let in_put_ports = [|
    !jack_client_name ^":ctrl"
  |]

  let licence = ref "
  Copyright (c) 2007, Sebastien MONDET                               \n\
  \n\
  Permission is hereby granted, free of charge, to any person        \n\
  obtaining a copy of this software and associated documentation     \n\
  files (the \"Software\"), to deal in the Software without            \n\
  restriction, including without limitation the rights to use,       \n\
  copy, modify, merge, publish, distribute, sublicense, and/or sell  \n\
  copies of the Software, and to permit persons to whom the          \n\
  Software is furnished to do so, subject to the following           \n\
  conditions:                                                        \n\
  \n\
  The above copyright notice and this permission notice shall be     \n\
  included in all copies or substantial portions of the Software.    \n\
  \n\
  THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,    \n\
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES    \n\
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND           \n\
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT        \n\
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,       \n\
  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING       \n\
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR      \n\
  OTHER DEALINGS IN THE SOFTWARE.                                    \n\
  "
end

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

let status_color_of_state = function
  | Tracker.Automata.AS_On       -> "Play","#00FF00"
  | Tracker.Automata.AS_Off      -> "Stop","#FFFFFF"
  | Tracker.Automata.AS_SchedOn  -> "S->P","#00AA00"
  | Tracker.Automata.AS_SchedOff -> "P->S","#00BB00"

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
let units_to_length b q t pqn = (
  (b * 4 * pqn) + (q * pqn) + t
)

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
    !App.app_name ^ " " ^ !App.version ^(
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


let midi_status_string_value = [
  ("NoteOFF           (0x80)", 0x80 ) ; 
  ("NoteON            (0x90)", 0x90 ) ; 
  ("AfterTouch        (0xA0)", 0xA0 ) ; 
  ("ControlChange     (0xB0)", 0xB0 ) ; 
  ("ProgramChange     (0xC0)", 0xC0 ) ; 
  ("ChannelAfterTouch (0xD0)", 0xD0 ) ; 
  ("PitchRange        (0xE0)", 0xE0 ) ; 
]

let global_available_midi_events = 
  ""::(fst (List.split midi_status_string_value))

let midi_status_of_string str = 
  if str = "" then -1 else 
    (snd (List.find (fun (a,b) -> a = str) midi_status_string_value))

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
  ""; "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9";
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






